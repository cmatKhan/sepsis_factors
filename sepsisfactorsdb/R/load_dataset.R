# Loading one dataset's results into the database. Everything about the
# dataset is written in one transaction. Rows arrive keyed by natural keys
# (fit_key, factor_index, ...); this resolves them to integer ids. Fits are
# upserted on fit_key, so a fit keeps its fit_id across loads; everything that
# hangs off a fit is deleted and inserted again.

# Dataset-level tables of the repeated-measures analysis (all keyed by
# dataset_id), cleared on each load.
VARIANCE_DATASET_TABLES <- c(
  "dataset_design",
  "samples",
  "variance_summary",
  "variance_terms",
  "variance_categories",
  "variance_bins",
  "trait_variance",
  "composition_correlations",
  "de_contrasts",
  "variance_time_genes"
)

#' Keep the columns of a results table that the database stores
#'
#' Results tables may carry columns the database does not hold (the `extra`
#' columns of the contract) or leave out optional ones, which stay `NULL`.
#'
#' @param df A results table.
#' @param table Its name.
#' @param keep Natural-key columns to keep as well, for resolving to ids.
#' @noRd
db_columns <- function(df, table, keep = character()) {
  df[, intersect(names(df), c(keep, schema_columns()[[table]])), drop = FALSE]
}

#' Turn artifact file names into the paths the database stores
#'
#' @noRd
store_paths <- function(df, table, dataset_id) {
  for (col in RESULT_TABLES[[table]]$paths) {
    ok <- !is.na(df[[col]])
    df[[col]][ok] <- artifact_rel(dataset_id, df[[col]][ok])
  }
  df
}

#' Replace a table's fit_key column with fit_id
#'
#' @noRd
with_fit_id <- function(df, fid, key = "fit_key") {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  df$fit_id <- fid(df[[key]])
  df[, c("fit_id", setdiff(names(df), c("fit_id", key))), drop = FALSE]
}

#' The artifact files the database points at
#'
#' The full loadings and scores of each fit (`POINTER_KINDS`) and the pattern
#' driver results. The other artifacts stay in the results output; the derive
#' step reads them from there.
#'
#' @return Character vector of file names in the results `artifacts/` directory.
#' @noRd
pointer_artifact_files <- function(tables) {
  fa <- tables[["fit_artifacts"]]
  files <- c(
    if (!is.null(fa)) fa$path[fa$kind %in% POINTER_KINDS],
    stats::na.omit(tables[["pattern_drivers"]][["path"]])
  )
  unique(files)
}

#' Load one dataset's results into the database
#'
#' Validates nothing itself (see [validate_results()]); [ingest_results()]
#' does both. In one transaction it upserts the dataset and its methods,
#' deletes everything hanging off the dataset's fits and its dataset-level
#' rows, upserts the fits on `fit_key` (a fit keeps its `fit_id`; fits no
#' longer in the results are dropped), then inserts every child table,
#' resolving natural keys to ids. The full loadings and scores and the pattern
#' driver files are placed first. The tables built from the full artifacts are
#' made afterwards by [derive_dataset()].
#'
#' @param results_root The results directory.
#' @param db_path Path to the SQLite file (created if new).
#' @param dataset_id Dataset id.
#' @param artifact_mode How artifact files get from the results into the
#'   database tree: `"link"` (hard link, copying across filesystems), `"copy"`,
#'   `"move"` or `"symlink"` (absolute symbolic link; the results must stay
#'   where they are).
#' @param artifact_base Path of the database whose directory holds
#'   `stability_artifacts/`. It is `db_path` unless `db_path` is a part file of
#'   a parallel build ([merge_parts()]), whose artifacts belong beside the final
#'   database.
#' @return A list: `dataset_id`, `fits` (counts by status), `written_at`.
#' @export
load_dataset <- function(results_root, db_path, dataset_id, artifact_mode = "link", artifact_base = db_path) {
  ddir <- results_dataset_dir(results_root, dataset_id)
  files <- list_table_files(file.path(ddir, "tables"))
  tables <- lapply(files, read_result_table)
  tab <- function(name) tables[[name]]
  now <- utc_now()

  art_files <- pointer_artifact_files(tables)
  place_artifacts(
    file.path(ddir, "artifacts", art_files),
    file.path(artifacts_dir(artifact_base, dataset_id), art_files),
    artifact_mode
  )

  con <- open_stability_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  q <- function(sql, ...) DBI::dbExecute(con, sql, params = list(...))
  id <- dataset_id

  with_transaction(con, {
    # ---- dataset + methods ----
    ds <- db_columns(tab("datasets"), "datasets")
    for (col in setdiff(contract_columns("datasets", include_optional = TRUE), names(ds))) {
      if (col %in% schema_columns()$datasets) ds[[col]] <- NA
    }
    cols <- names(ds)
    DBI::dbExecute(
      con,
      sprintf(
        "INSERT INTO datasets (%s) VALUES (%s) ON CONFLICT(dataset_id) DO UPDATE SET %s",
        paste(cols, collapse = ", "),
        paste(rep("?", length(cols)), collapse = ", "),
        paste(sprintf("%s = excluded.%s", setdiff(cols, "dataset_id"), setdiff(cols, "dataset_id")), collapse = ", ")
      ),
      params = unname(as.list(ds))
    )
    mt <- tab("methods")
    for (i in seq_len(nrow(mt))) {
      d <- method_display(mt$method[i])
      q(
        "INSERT INTO methods (method, family, sign_ambiguous, has_loadings, label, color, display_order, blurb)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)
         ON CONFLICT(method) DO UPDATE SET family = excluded.family,
           sign_ambiguous = excluded.sign_ambiguous, has_loadings = excluded.has_loadings,
           label = excluded.label, color = excluded.color, display_order = excluded.display_order,
           blurb = excluded.blurb",
        mt$method[i],
        mt$family[i],
        as.integer(mt$sign_ambiguous[i]),
        as.integer(mt$has_loadings[i]),
        d$label,
        d$color,
        d$order,
        d$blurb
      )
    }

    # ---- clear what hangs off this dataset's fits, and its dataset-level rows ----
    in_ds <- "(SELECT fit_id FROM fits WHERE dataset_id = ?)"
    q(sprintf("DELETE FROM factors WHERE fit_id IN %s", in_ds), id) # cascades factor-level rows
    for (t in c(
      "fit_params",
      "fit_metrics",
      "fit_artifacts",
      "gene_modules",
      "module_kme_summary",
      "module_kme_histograms"
    )) {
      q(sprintf("DELETE FROM %s WHERE fit_id IN %s", t, in_ds), id)
    }
    q(sprintf("DELETE FROM fit_pairs WHERE fit_a IN %s OR fit_b IN %s", in_ds, in_ds), id, id)
    for (t in c(
      "matrix_diagnostics",
      "wgcna_sft",
      "gene_significance",
      "icasso_clusters",
      VARIANCE_DATASET_TABLES
    )) {
      q(sprintf("DELETE FROM %s WHERE dataset_id = ?", t), id)
    }

    # ---- fits: upsert by fit_key, drop fits no longer in the results ----
    fits <- db_columns(tab("fits"), "fits")
    DBI::dbWriteTable(con, "new_fits", fits, temporary = TRUE, overwrite = TRUE)
    q("DELETE FROM fits WHERE dataset_id = ? AND fit_key NOT IN (SELECT fit_key FROM temp.new_fits)", id)
    fit_cols <- names(fits)
    DBI::dbExecute(
      con,
      sprintf(
        "INSERT INTO fits (%s) SELECT %s FROM temp.new_fits WHERE true
         ON CONFLICT(fit_key) DO UPDATE SET %s",
        paste(fit_cols, collapse = ", "),
        paste(fit_cols, collapse = ", "),
        paste(sprintf("%s = excluded.%s", setdiff(fit_cols, "fit_key"), setdiff(fit_cols, "fit_key")), collapse = ", ")
      )
    )
    DBI::dbExecute(con, "DROP TABLE temp.new_fits")
    fit_ids <- id_lookup(
      DBI::dbGetQuery(con, "SELECT fit_id, fit_key FROM fits WHERE dataset_id = ?", params = list(id)),
      "fit_id",
      "fit_key"
    )
    fid <- function(keys) map_ids(keys, fit_ids, "fit")

    # ---- per-fit long tables ----
    append_rows(con, "fit_params", with_fit_id(tab("fit_params"), fid))
    append_rows(con, "fit_metrics", with_fit_id(tab("fit_metrics"), fid))
    fa <- tab("fit_artifacts")
    if (!is.null(fa)) {
      fa <- store_paths(fa[fa$kind %in% POINTER_KINDS, , drop = FALSE], "fit_artifacts", id)
      append_rows(con, "fit_artifacts", with_fit_id(fa, fid))
    }

    # ---- factors ----
    append_rows(con, "factors", with_fit_id(db_columns(tab("factors"), "factors", "fit_key"), fid))
    factor_ids <- id_lookup(
      DBI::dbGetQuery(
        con,
        "SELECT x.factor_id, f.fit_key, x.factor_index FROM factors x
         JOIN fits f ON f.fit_id = x.fit_id WHERE f.dataset_id = ?",
        params = list(id)
      ),
      "factor_id",
      c("fit_key", "factor_index")
    )
    xid <- function(keys, idx) map_ids(paste(keys, idx), factor_ids, "factor")
    # Rows for factors that are not in the factors table (e.g. a factor
    # dropped upstream) cannot be linked: drop them, with a warning.
    known_factors <- function(df, table) {
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }
      known <- paste(df$fit_key, df$factor_index) %in% names(factor_ids)
      if (!all(known)) {
        cli::cli_warn("Dropped {sum(!known)} {.field {table}} row{?s} for factors not in the database.")
      }
      df[known, , drop = FALSE]
    }

    # ---- pair stability ----
    load_pairs(con, tab, fid, xid)

    # ---- WGCNA per-fit kME ----
    for (t in c("gene_modules", "module_kme_summary", "module_kme_histograms")) {
      append_rows(con, t, with_fit_id(db_columns(tab(t), t, "fit_key"), fid))
    }

    # ---- representative-fit analyses ----
    pd <- tab("pattern_drivers")
    if (!is.null(pd) && nrow(pd)) {
      pd <- store_paths(pd, "pattern_drivers", id)
      append_rows(
        con,
        "pattern_drivers",
        cbind(
          data.frame(factor_id = xid(pd$fit_key, pd$factor_index)),
          db_columns(pd, "pattern_drivers")
        )
      )
    }

    # ---- ICASSO ----
    cl <- tab("icasso_clusters")
    if (!is.null(cl)) append_rows(con, "icasso_clusters", db_columns(cl, "icasso_clusters"))

    # ---- enrichment ----
    eq <- known_factors(tab("enrichment_queries"), "enrichment_queries")
    if (!is.null(eq) && nrow(eq)) {
      eq$query_id <- seq.int(next_id(con, "enrichment_queries", "query_id"), length.out = nrow(eq))
      append_rows(
        con,
        "enrichment_queries",
        data.frame(
          query_id = eq$query_id,
          factor_id = xid(eq$fit_key, eq$factor_index),
          query_type = eq$query_type,
          direction = eq$direction
        )
      )
      er <- tab("enrichment_results")
      if (!is.null(er) && nrow(er)) {
        qid <- stats::setNames(
          eq$query_id,
          paste(eq$fit_key, eq$factor_index, eq$query_type, eq$direction)
        )
        rq <- unname(qid[paste(er$fit_key, er$factor_index, er$query_type, er$direction)])
        er <- er[!is.na(rq), , drop = FALSE]
        rq <- rq[!is.na(rq)]
        append_rows(
          con,
          "enrichment_results",
          cbind(
            data.frame(query_id = rq),
            db_columns(er, "enrichment_results")
          )
        )
      }
    }

    # ---- repeated measures and dataset-level tables ----
    for (t in VARIANCE_DATASET_TABLES) {
      df <- tab(t)
      if (!is.null(df)) append_rows(con, t, db_columns(df, t))
    }
    md <- tab("matrix_diagnostics")
    if (!is.null(md)) append_rows(con, "matrix_diagnostics", db_columns(md, "matrix_diagnostics"))
    ws <- tab("wgcna_sft")
    if (!is.null(ws)) append_rows(con, "wgcna_sft", db_columns(ws, "wgcna_sft"))
    append_rows(con, "gene_significance", tab("gene_significance"))
  })

  counts <- DBI::dbGetQuery(
    con,
    "SELECT status, COUNT(*) AS n FROM fits WHERE dataset_id = ? GROUP BY status",
    params = list(id)
  )
  list(dataset_id = id, fits = stats::setNames(counts$n, counts$status), written_at = now)
}

#' Load the pair-stability tables of one dataset
#'
#' `fit_pairs` stores `fit_a < fit_b` by id: a pair given the other way round
#' is swapped (counts and matched factors with it).
#'
#' @noRd
load_pairs <- function(con, tab, fid, xid) {
  fp <- tab("fit_pairs")
  if (is.null(fp) || nrow(fp) == 0) {
    return(invisible(NULL))
  }
  ia <- fid(fp$fit_a_key)
  ib <- fid(fp$fit_b_key)
  swap <- ia > ib
  fp$fit_pair_id <- seq.int(next_id(con, "fit_pairs", "fit_pair_id"), length.out = nrow(fp))
  pair_id <- stats::setNames(fp$fit_pair_id, paste(fp$fit_a_key, fp$fit_b_key))
  append_rows(
    con,
    "fit_pairs",
    data.frame(
      fit_pair_id = fp$fit_pair_id,
      fit_a = ifelse(swap, ib, ia),
      fit_b = ifelse(swap, ia, ib),
      same_rank = fp$same_rank,
      ari = fp$ari
    )
  )

  mt <- tab("factor_matches")
  if (!is.null(mt) && nrow(mt)) {
    xa <- xid(mt$fit_a_key, mt$factor_a_index)
    xb <- xid(mt$fit_b_key, mt$factor_b_index)
    sw <- fid(mt$fit_a_key) > fid(mt$fit_b_key)
    mt$match_id <- seq.int(next_id(con, "factor_matches", "match_id"), length.out = nrow(mt))
    append_rows(
      con,
      "factor_matches",
      data.frame(
        match_id = mt$match_id,
        fit_pair_id = unname(pair_id[paste(mt$fit_a_key, mt$fit_b_key)]),
        factor_a = ifelse(sw, xb, xa),
        factor_b = ifelse(sw, xa, xb)
      )
    )
    match_id <- stats::setNames(mt$match_id, paste(mt$fit_a_key, mt$fit_b_key, mt$factor_a_index))
    sc <- tab("match_scores")
    if (!is.null(sc) && nrow(sc)) {
      append_rows(
        con,
        "match_scores",
        data.frame(
          match_id = map_ids(paste(sc$fit_a_key, sc$fit_b_key, sc$factor_a_index), match_id, "factor match"),
          metric = sc$metric,
          value = sc$value,
          runner_up = sc$runner_up,
          margin = sc$margin
        )
      )
    }
  }

  nl <- tab("fit_pair_null")
  if (!is.null(nl) && nrow(nl)) {
    append_rows(
      con,
      "fit_pair_null",
      cbind(
        data.frame(fit_pair_id = map_ids(paste(nl$fit_a_key, nl$fit_b_key), pair_id, "fit pair")),
        nl[, c("metric", "p95")]
      )
    )
  }
  invisible(NULL)
}
