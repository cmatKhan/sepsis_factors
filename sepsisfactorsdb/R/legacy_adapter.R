# TEMPORARY legacy adapter: reshapes a schema-3 database and its
# stability_artifacts/ tree (the original project's output, the form in which
# the computed results exist today) into a results output that ingest_results()
# accepts. Nothing is computed; surrogate ids become natural keys and files are
# linked. It goes when `sepsisfactors` writes the contract itself (see the
# "TEMPORARY patches" section of the root TODO.md).

# Artifact kinds the adapter links: the ones the database points at (loadings,
# scores, projections, pattern drivers) and the ones the derive step reads at
# load time (diag, dendrogram, and the dataset-level kinds below).
LEGACY_FIT_KINDS <- c("loadings", "scores", "diag", "dendrogram")
LEGACY_DATASET_KINDS <- c("sample_metadata", "feature_metadata", "variance_partition")

#' Convert a legacy tree to a results output
#'
#' Reads `<legacy_root>/stability.sqlite` (schema 3, opened read-only and
#' immutable, so a read-only mount works) and `<legacy_root>/stability_artifacts/`
#' and writes a results output to `out_root`: a manifest, one Parquet file per
#' table with natural keys in place of ids (following [contract_columns()]),
#' and the artifacts the database points at or the derive step reads. The
#' dataset family, its position in the family and the Hugging Face collection
#' come from `config_dir`, and the composition markers become the
#' `composition_markers` table.
#'
#' @section Temporary:
#' This function exists only until `sepsisfactors` writes the results output
#' (version 2) itself; then it is deleted. It reshapes, it computes nothing.
#'
#' @param legacy_root Directory holding `stability.sqlite` and
#'   `stability_artifacts/`.
#' @param out_root Directory to write (created; must be empty or absent).
#' @param config_dir A `sepsisfactors/inst`-style directory:
#'   `config/dataset_families.yml`, `config/composition_markers.yml` and
#'   `datasets/<id>/config.yml`.
#' @param datasets Dataset ids to convert; `NULL` for all. Projections and
#'   gene-space agreement are kept only when source and target are both
#'   converted.
#' @param artifact_mode How artifact files get into `out_root`: `"auto"`
#'   (hard links when `out_root` is on the legacy tree's filesystem, otherwise
#'   symbolic links), `"link"` (hard links; an error if impossible),
#'   `"symlink"` (absolute symbolic links to the source files, which must stay
#'   in place) or `"copy"`. Copies are never made unless asked for.
#' @return `out_root`, invisibly.
#' @export
legacy_to_results <- function(
  legacy_root,
  out_root,
  config_dir,
  datasets = NULL,
  artifact_mode = c("auto", "link", "symlink", "copy")
) {
  artifact_mode <- match.arg(artifact_mode)
  db_file <- file.path(legacy_root, "stability.sqlite")
  if (!file.exists(db_file)) {
    cli::cli_abort("No {.file stability.sqlite} in {.file {legacy_root}}.")
  }
  if (dir.exists(out_root) && length(list.files(out_root))) {
    cli::cli_abort("{.file {out_root}} is not empty.")
  }
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    paste0("file:", normalizePath(db_file), "?immutable=1"),
    flags = RSQLite::SQLITE_RO,
    synchronous = NULL
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  version <- db_schema_version(con)
  if (!identical(version, 3L)) {
    cli::cli_abort("{.file {db_file}} has schema version {version}; the adapter reads version 3.")
  }
  all_ids <- DBI::dbGetQuery(con, "SELECT dataset_id FROM datasets ORDER BY dataset_id")$dataset_id
  ids <- datasets %||% all_ids
  absent <- setdiff(ids, all_ids)
  if (length(absent)) {
    cli::cli_abort(
      "{cli::qty(length(absent))}Dataset{?s} {.val {absent}} {?is/are} not in {.file {db_file}}."
    )
  }
  art_root <- file.path(legacy_root, "stability_artifacts")
  mode <- resolve_legacy_mode(artifact_mode, art_root, out_root)
  cfg <- read_legacy_config(config_dir)

  for (id in ids) legacy_dataset(con, id, out_root, art_root, mode, cfg)
  legacy_cross(con, ids, out_root, art_root, mode, cfg)

  methods <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT DISTINCT method FROM fits WHERE dataset_id IN (%s)",
      paste(DBI::dbQuoteString(con, ids), collapse = ",")
    )
  )$method
  method_versions <- stats::setNames(
    lapply(methods, function(m) list(method_output_version = 1L)),
    methods
  )
  jsonlite::write_json(
    list(
      results_output_version = RESULTS_OUTPUT_VERSION,
      writer = list(
        package = "sepsisfactorsdb",
        version = as.character(utils::packageVersion("sepsisfactorsdb")),
        note = "legacy adapter"
      ),
      created_at = utc_now(),
      datasets = ids,
      methods = method_versions
    ),
    file.path(out_root, "manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
  invisible(out_root)
}

#' Choose how to put files into the results output
#'
#' @noRd
resolve_legacy_mode <- function(mode, art_root, out_root) {
  if (mode != "auto") {
    return(mode)
  }
  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)
  probe <- list.files(art_root, recursive = TRUE, full.names = TRUE)[1]
  test <- file.path(out_root, ".link-test")
  same <- !is.na(probe) && isTRUE(suppressWarnings(file.link(probe, test)))
  unlink(test)
  if (same) "link" else "symlink"
}

#' Put one artifact file into the results output
#'
#' @param from Source file.
#' @param to Destination file.
#' @param mode `"link"`, `"symlink"` or `"copy"`; never falls back to another.
#' @noRd
legacy_place <- function(from, to, mode) {
  if (!file.exists(from)) {
    cli::cli_abort("Legacy artifact {.file {from}} is missing.")
  }
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  unlink(to)
  ok <- switch(mode,
    link = suppressWarnings(file.link(from, to)),
    symlink = suppressWarnings(file.symlink(normalizePath(from), to)),
    copy = file.copy(from, to)
  )
  if (!ok) {
    cli::cli_abort(
      c(
        "Could not {mode} {.file {from}} to {.file {to}}.",
        "i" = paste0(
          "Choose another {.arg artifact_mode}; files are never copied unless ",
          "{.code artifact_mode = \"copy\"}."
        )
      )
    )
  }
  invisible(to)
}

#' Read the config files the adapter uses
#'
#' @noRd
read_legacy_config <- function(config_dir) {
  read <- function(...) {
    path <- file.path(config_dir, ...)
    if (file.exists(path)) yaml::read_yaml(path) else NULL
  }
  list(
    dir = config_dir,
    families = read("config", "dataset_families.yml"),
    markers = read("config", "composition_markers.yml")
  )
}

#' Family, position in the family and collection of a dataset
#'
#' @noRd
legacy_dataset_config <- function(cfg, id) {
  family <- NA_character_
  position <- NA_integer_
  for (nm in names(cfg$families)) {
    hit <- match(id, unlist(cfg$families[[nm]]))
    if (!is.na(hit)) {
      family <- nm
      position <- hit
      break
    }
  }
  own <- file.path(cfg$dir, "datasets", id, "config.yml")
  collection <- if (file.exists(own)) yaml::read_yaml(own)$dataset$hf_collection else NULL
  list(
    family = family,
    family_position = as.integer(position),
    hf_collection = collection %||% NA_character_
  )
}

#' Write one results table, checking it against the contract
#'
#' @param df Rows.
#' @param table Table name.
#' @param dir `datasets/<id>` or `cross_dataset` directory.
#' @param required Write even when empty.
#' @noRd
write_legacy_table <- function(df, table, dir, required = FALSE) {
  if (nrow(df) == 0L && !required) {
    return(invisible(0L))
  }
  want <- contract_columns(table, include_optional = TRUE)
  missing <- setdiff(contract_columns(table), names(df))
  if (length(missing)) {
    cli::cli_abort("Legacy data for {.val {table}} lacks {.field {missing}}.")
  }
  df <- df[, intersect(want, names(df)), drop = FALSE]
  dir.create(file.path(dir, "tables"), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(df, file.path(dir, "tables", paste0(table, ".parquet")))
  invisible(nrow(df))
}

#' Convert one dataset
#'
#' @noRd
legacy_dataset <- function(con, id, out_root, art_root, mode, cfg) {
  dir <- file.path(out_root, "datasets", id)
  dir.create(file.path(dir, "artifacts"), recursive = TRUE, showWarnings = FALSE)
  # a table the legacy database lacks is an empty table
  q <- function(sql) {
    tryCatch(
      DBI::dbGetQuery(con, sql, params = list(id)),
      error = function(e) if (grepl("no such table", conditionMessage(e))) data.frame() else stop(e)
    )
  }
  base <- function(x) ifelse(is.na(x), NA_character_, basename(x))
  # a table keyed by dataset_id, copied as it is
  same <- function(table) q(sprintf("SELECT * FROM %s WHERE dataset_id = ?", table))
  # a table keyed by fit_id: fit_key first
  by_fit <- function(table) {
    q(
      sprintf(
        "SELECT f.fit_key, t.* FROM %s t JOIN fits f ON f.fit_id = t.fit_id WHERE f.dataset_id = ?",
        table
      )
    )
  }
  write <- function(df, table, required = FALSE) write_legacy_table(df, table, dir, required)

  ds <- same("datasets")
  cfg_d <- legacy_dataset_config(cfg, id)
  ds$family <- cfg_d$family
  ds$family_position <- cfg_d$family_position
  ds$hf_collection <- cfg_d$hf_collection
  write(ds, "datasets", required = TRUE)
  write(
    q(
      paste0(
        "SELECT * FROM methods WHERE method IN (SELECT DISTINCT method FROM fits WHERE ",
        "dataset_id = ?)"
      )
    ),
    "methods",
    required = TRUE
  )
  write(same("fits"), "fits", required = TRUE)

  for (t in c(
    "matrix_diagnostics", "wgcna_sft", "gene_significance", "dataset_design", "samples",
    "variance_summary", "variance_terms", "variance_categories", "variance_bins",
    "trait_variance", "composition_correlations", "de_contrasts", "variance_time_genes"
  )) {
    write(same(t), t)
  }
  for (
    t in c(
      "fit_params",
      "fit_metrics",
      "factors",
      "gene_modules",
      "module_kme_summary",
      "module_kme_histograms"
    )
  ) {
    write(by_fit(t), t)
  }

  fa <- by_fit("fit_artifacts")
  fa <- fa[fa$kind %in% LEGACY_FIT_KINDS, , drop = FALSE]
  da <- same("dataset_artifacts")
  da <- da[da$kind %in% LEGACY_DATASET_KINDS, , drop = FALSE]
  pd <- q(
    "SELECT f.fit_key, x.factor_index, d.* FROM pattern_drivers d
     JOIN factors x ON x.factor_id = d.factor_id JOIN fits f ON f.fit_id = x.fit_id
     WHERE f.dataset_id = ?"
  )
  for (p in c(fa$path, da$path, pd$path)) {
    legacy_place(file.path(art_root, "..", p), file.path(dir, "artifacts", basename(p)), mode)
  }
  fa$path <- base(fa$path)
  da$path <- base(da$path)
  pd$path <- base(pd$path)
  write(fa, "fit_artifacts")
  write(da, "dataset_artifacts")
  write(pd, "pattern_drivers")

  write(
    q(paste0(
      "SELECT fa.fit_key AS fit_a_key, fb.fit_key AS fit_b_key, p.same_rank, p.n_factors_a, ",
      "p.n_factors_b, p.ari\n",
      "       FROM fit_pairs p JOIN fits fa ON fa.fit_id = p.fit_a JOIN fits fb ON fb.fit_id = ",
      "p.fit_b\n",
      "       WHERE fa.dataset_id = ?"
    )),
    "fit_pairs"
  )
  pair_join <- "FROM factor_matches m
       JOIN fit_pairs p ON p.fit_pair_id = m.fit_pair_id
       JOIN fits fa ON fa.fit_id = p.fit_a JOIN fits fb ON fb.fit_id = p.fit_b
       JOIN factors xa ON xa.factor_id = m.factor_a JOIN factors xb ON xb.factor_id = m.factor_b"
  write(
    q(paste(
      "SELECT fa.fit_key AS fit_a_key, fb.fit_key AS fit_b_key, xa.factor_index AS factor_a_index,
              xb.factor_index AS factor_b_index", pair_join, "WHERE fa.dataset_id = ?"
    )),
    "factor_matches"
  )
  write(
    q(paste(
      "SELECT fa.fit_key AS fit_a_key, fb.fit_key AS fit_b_key, xa.factor_index AS factor_a_index,
              s.metric, s.value, s.runner_up, s.margin", pair_join,
      "JOIN match_scores s ON s.match_id = m.match_id WHERE fa.dataset_id = ?"
    )),
    "match_scores"
  )
  write(
    q("SELECT fa.fit_key AS fit_a_key, fb.fit_key AS fit_b_key, n.*
       FROM fit_pair_null n JOIN fit_pairs p ON p.fit_pair_id = n.fit_pair_id
       JOIN fits fa ON fa.fit_id = p.fit_a JOIN fits fb ON fb.fit_id = p.fit_b
       WHERE fa.dataset_id = ?"),
    "fit_pair_null"
  )
  ic <- q(
    "SELECT c.*, f.fit_key AS centrotype_fit_key, x.factor_index AS centrotype_factor_index
     FROM icasso_clusters c LEFT JOIN factors x ON x.factor_id = c.centrotype_factor_id
     LEFT JOIN fits f ON f.fit_id = x.fit_id WHERE c.dataset_id = ?"
  )
  ic$dendro_file <- base(ic$dendro_file)
  write(ic, "icasso_clusters")
  write(
    q("SELECT f.fit_key, x.factor_index, q.query_type, q.direction, q.query_size, q.queried_at
       FROM enrichment_queries q JOIN factors x ON x.factor_id = q.factor_id
       JOIN fits f ON f.fit_id = x.fit_id WHERE f.dataset_id = ?"),
    "enrichment_queries"
  )
  write(
    q("SELECT f.fit_key, x.factor_index, q.query_type, q.direction, r.*
       FROM enrichment_results r JOIN enrichment_queries q ON q.query_id = r.query_id
       JOIN factors x ON x.factor_id = q.factor_id JOIN fits f ON f.fit_id = x.fit_id
       WHERE f.dataset_id = ?"),
    "enrichment_results"
  )
  invisible(NULL)
}

#' Convert the cross-dataset tables
#'
#' @noRd
legacy_cross <- function(con, ids, out_root, art_root, mode, cfg) {
  dir <- file.path(out_root, "cross_dataset")
  dir.create(file.path(dir, "artifacts"), recursive = TRUE, showWarnings = FALSE)
  in_ids <- paste(DBI::dbQuoteString(con, ids), collapse = ",")
  q <- function(sql) {
    tryCatch(
      DBI::dbGetQuery(con, sql),
      error = function(e) if (grepl("no such table", conditionMessage(e))) data.frame() else stop(e)
    )
  }
  pr <- q(
    sprintf(
      paste0(
        "SELECT f.fit_key AS source_fit_key, p.* FROM projections p JOIN fits f ON f.fit_id = ",
        "p.source_fit_id\n",
        "       WHERE f.dataset_id IN (%s) AND p.target_dataset_id IN (%s)"
      ),
      in_ids, in_ids
    )
  )
  for (p in pr$path) {
    legacy_place(file.path(art_root, "..", p), file.path(dir, "artifacts", basename(p)), mode)
  }
  if (nrow(pr)) pr$path <- basename(pr$path)
  write_legacy_table(pr, "projections", dir)
  write_legacy_table(
    q(
      sprintf(
        paste0(
          "SELECT a.fit_key AS source_fit_key, b.fit_key AS target_fit_key, g.n_genes_matched, ",
          "g.mean_abs_diagonal\n",
          "         FROM gene_space_agreement g JOIN fits a ON a.fit_id = g.source_fit_id JOIN ",
          "fits b ON b.fit_id = g.target_fit_id\n",
          "         WHERE a.dataset_id IN (%s) AND b.dataset_id IN (%s)"
        ),
        in_ids, in_ids
      )
    ),
    "gene_space_agreement",
    dir
  )
  write_legacy_table(q("SELECT * FROM time_gene_recurrence"), "time_gene_recurrence", dir)
  markers <- cfg$markers
  if (length(markers)) {
    write_legacy_table(
      data.frame(
        marker_set = rep(names(markers), lengths(markers)),
        gene = as.character(unlist(markers, use.names = FALSE))
      ),
      "composition_markers",
      dir
    )
  }
  if (!dir.exists(file.path(dir, "tables"))) {
    unlink(dir, recursive = TRUE)
  }
  invisible(NULL)
}
