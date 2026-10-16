# The results-output contract ("contract A"). One Parquet file per table; the
# columns are the schema table's columns with each surrogate id replaced by its
# natural key. RESULT_TABLES says how each table differs from the schema;
# contract_columns() applies it to the schema, so the contract cannot drift
# from schema.sql.
#
# For each table:
#   scope     "dataset" (datasets/<id>/tables/) or "cross" (cross_dataset/tables/)
#   drop      schema columns that are surrogate ids or loader-filled, absent from the results
#   add       natural-key columns present in the results instead (placed first)
#   optional  schema columns the results may omit (NULL when absent)
#   extra     columns the results may carry that the database does not store
#   paths     columns holding an artifact file name (relative to the artifacts/
#             directory next to tables/); the loader stores
#             stability_artifacts/<dataset_id>/<file name>
#   loaded    FALSE for a results table the database no longer holds (kept in
#             the contract so the results output stays complete); `columns`
#             then lists its columns, all required
RESULT_TABLES <- list(
  datasets = list(
    scope = "dataset",
    optional = c("family", "family_position", "hf_collection"),
    extra = c(
      "description", "n_samples", "n_genes", "ensembl_col",
      "sample_metadata_source", "feature_metadata_source", "written_at"
    )
  ),
  methods = list(scope = "dataset", drop = c("label", "color", "display_order", "blurb")),
  dataset_artifacts = list(
    scope = "dataset",
    loaded = FALSE,
    columns = c("dataset_id", "kind", "path"),
    paths = "path"
  ),
  matrix_diagnostics = list(
    scope = "dataset",
    extra = c(
      "val_min", "val_max", "val_mean", "skew_p50", "skew_p95", "kurtosis_p50",
      "kurtosis_p95", "min_singular_value", "max_singular_value", "condition_number",
      "max_sample_cor", "computed_at"
    )
  ),
  fits = list(
    scope = "dataset",
    drop = "fit_id",
    add = "fit_key",
    extra = c("error", "written_at")
  ),
  fit_params = list(scope = "dataset", drop = "fit_id", add = "fit_key"),
  fit_metrics = list(scope = "dataset", drop = "fit_id", add = "fit_key"),
  fit_artifacts = list(
    scope = "dataset",
    drop = "fit_id",
    add = "fit_key",
    paths = "path"
  ),
  factors = list(scope = "dataset", drop = c("factor_id", "fit_id"), add = c("fit_key")),
  fit_pairs = list(
    scope = "dataset",
    drop = c("fit_pair_id", "fit_a", "fit_b"),
    add = c("fit_a_key", "fit_b_key"),
    extra = c("n_factors_a", "n_factors_b")
  ),
  factor_matches = list(
    scope = "dataset",
    drop = c("match_id", "fit_pair_id", "factor_a", "factor_b"),
    add = c("fit_a_key", "fit_b_key", "factor_a_index", "factor_b_index")
  ),
  match_scores = list(
    scope = "dataset",
    drop = "match_id",
    add = c("fit_a_key", "fit_b_key", "factor_a_index")
  ),
  fit_pair_null = list(
    scope = "dataset",
    drop = "fit_pair_id",
    add = c("fit_a_key", "fit_b_key"),
    extra = c("n", "mean", "sd", "median", "max")
  ),
  similarity_histograms = list(
    scope = "dataset",
    loaded = FALSE,
    columns = c(
      "dataset_id", "method", "level_a", "level_b", "metric", "bin", "bin_lo", "bin_hi",
      "n_matched", "n_unmatched"
    )
  ),
  gene_modules = list(scope = "dataset", drop = "fit_id", add = "fit_key"),
  module_kme_summary = list(
    scope = "dataset",
    drop = "fit_id",
    add = "fit_key",
    extra = c("sd", "p99")
  ),
  module_kme_histograms = list(scope = "dataset", drop = "fit_id", add = "fit_key"),
  wgcna_sft = list(
    scope = "dataset",
    extra = c("slope", "truncated_r_sq", "mean_k", "median_k", "max_k")
  ),
  gene_significance = list(scope = "dataset"),
  icasso_clusters = list(
    scope = "dataset",
    extra = c("centrotype_fit_key", "centrotype_factor_index", "dendro_file")
  ),
  icasso_membership = list(
    scope = "dataset",
    loaded = FALSE,
    columns = c("fit_key", "factor_index", "dataset_id", "rank", "cluster_id", "intra_sim")
  ),
  enrichment_queries = list(
    scope = "dataset",
    drop = c("query_id", "factor_id"),
    add = c("fit_key", "factor_index"),
    extra = c("query_size", "queried_at")
  ),
  enrichment_results = list(
    scope = "dataset",
    drop = "query_id",
    add = c("fit_key", "factor_index", "query_type", "direction"),
    extra = c("es", "log2err", "is_main_pathway")
  ),
  pattern_drivers = list(
    scope = "dataset",
    drop = c("driver_id", "factor_id"),
    add = c("fit_key", "factor_index"),
    extra = c("n_genes_considered", "n_significant_shared", "computed_at"),
    paths = "path"
  ),
  fit_redundancy = list(
    scope = "dataset",
    loaded = FALSE,
    columns = c(
      "fit_key", "max_offdiag_cosine", "median_offdiag_cosine", "n_factors_with_no_markers"
    )
  ),
  dataset_design = list(scope = "dataset"),
  samples = list(
    scope = "dataset",
    extra = c(
      "expr_mean", "expr_sd", "expr_q10", "expr_median", "expr_q90", "control_mean",
      "control_factor", "log2_lib_size", "norm_factor"
    )
  ),
  variance_summary = list(
    scope = "dataset",
    extra = c(
      "n_factor_genes", "share_time_gt05", "share_time_gt20", "between_vw_all", "time_vw_all",
      "residual_vw_all", "between_vw_factor_genes", "time_vw_factor_genes",
      "residual_vw_factor_genes", "between_vw_top", "time_vw_top", "residual_vw_top",
      "median_r2_nl_all", "median_r2_technical_all", "rho_sd_nl", "rho_mean_nl",
      "rho_sd_technical", "rho_mean_technical", "n_control_probes", "control_probes",
      "nl_group_fraction", "vp_seconds", "dream_seconds", "fitted_at"
    )
  ),
  variance_terms = list(scope = "dataset", extra = c("n_genes", "mean", "q10")),
  variance_categories = list(scope = "dataset"),
  variance_bins = list(scope = "dataset"),
  trait_variance = list(scope = "dataset"),
  composition_correlations = list(scope = "dataset"),
  de_contrasts = list(scope = "dataset"),
  factor_variance_models = list(
    scope = "dataset",
    loaded = FALSE,
    columns = c("dataset_id", "method", "formula", "n_factors")
  ),
  factor_variance = list(
    scope = "dataset",
    loaded = FALSE,
    columns = c("fit_key", "factor_index", "term", "category", "fraction")
  ),
  variance_time_genes = list(scope = "dataset"),
  projections = list(
    scope = "cross",
    drop = c("projection_id", "source_fit_id"),
    add = "source_fit_key",
    extra = c(
      "n_genes_matched", "n_samples", "mean_pval", "median_pval", "mean_pvar", "median_pvar",
      "n_components", "transfer_r2", "native_r2", "null_r2"
    ),
    paths = "path"
  ),
  gene_space_agreement = list(
    scope = "cross",
    drop = c("source_fit_id", "target_fit_id"),
    add = c("source_fit_key", "target_fit_key")
  ),
  time_gene_recurrence = list(scope = "cross"),
  composition_markers = list(scope = "cross")
)

# The artifact kinds whose files are placed in the database's own tree and
# pointed at by fit_artifacts. The other kinds (diag, dendrogram) and the
# dataset-level artifacts are read from the results output by the derive step.
POINTER_KINDS <- c("loadings", "scores")

# What each supported method's output must look like, per method_output_version.
# The loader checks a results tree against this before writing anything: the
# `methods` row must agree, every ok fit must have the required artifacts, and
# the required tables must exist. `scores_unless_bootstrap` lists methods whose
# bootstrap fits carry no scores. `display` (label, colour, order, blurb) is not
# part of the contract: the loader writes it to the `methods` table so readers
# need no copy of it. A new method, or a changed output of an existing one,
# adds an entry or a version here.
SUPPORTED_METHOD_OUTPUTS <- list(
  pca = list(
    versions = 1L,
    display = list(
      label = "PCA",
      color = "#4C78A8",
      order = 1L,
      blurb = paste0(
        "One deterministic fit at the largest rank; rank n is its first n components, ",
        "so there is no seed stability."
      )
    ),
    family = "seed_sweep",
    sign_ambiguous = TRUE,
    has_loadings = TRUE,
    fit_artifacts = c("loadings", "scores", "diag"),
    tables = character()
  ),
  spca = list(
    versions = 1L,
    display = list(
      label = "sPCA",
      color = "#72B7B2",
      order = 2L,
      blurb = paste0(
        "Deterministic given the penalty. Each rank is fit over a grid of penalties ",
        "(para); the one with the best Index of Sparseness is the representative."
      )
    ),
    family = "param_grid",
    sign_ambiguous = TRUE,
    has_loadings = TRUE,
    fit_artifacts = c("loadings", "scores", "diag"),
    tables = character()
  ),
  ica = list(
    versions = 1L,
    display = list(
      label = "ICA",
      color = "#F58518",
      order = 3L,
      blurb = paste0(
        "FastICA over a seed sweep plus bootstrap resamples (for ICASSO). Components ",
        "are matched across fits by |similarity|."
      )
    ),
    family = "seed_sweep",
    sign_ambiguous = TRUE,
    has_loadings = TRUE,
    fit_artifacts = c("loadings", "scores", "diag"),
    scores_unless_bootstrap = TRUE,
    tables = character()
  ),
  nmf = list(
    versions = 1L,
    display = list(
      label = "NMF",
      color = "#54A24B",
      order = 4L,
      blurb = "NMF over a seed sweep per rank. Each rank's representative is the lowest-error seed."
    ),
    family = "seed_sweep",
    sign_ambiguous = FALSE,
    has_loadings = TRUE,
    fit_artifacts = c("loadings", "scores", "diag"),
    tables = character()
  ),
  cogaps = list(
    versions = 1L,
    display = list(
      label = "CoGAPS",
      color = "#B279A2",
      order = 5L,
      blurb = paste0(
        "CoGAPS over a seed sweep per rank. Each rank's representative is the ",
        "lowest-error seed."
      )
    ),
    family = "seed_sweep",
    sign_ambiguous = FALSE,
    has_loadings = TRUE,
    fit_artifacts = c("loadings", "scores", "diag"),
    tables = character()
  ),
  wgcna = list(
    versions = 1L,
    display = list(
      label = "WGCNA",
      color = "#E45756",
      order = 6L,
      blurb = paste0(
        "One network per soft-threshold power; modules are compared across powers by ",
        "gene overlap (ARI)."
      )
    ),
    family = "param_grid",
    sign_ambiguous = FALSE,
    has_loadings = FALSE,
    fit_artifacts = c("scores", "diag"),
    tables = "gene_modules"
  )
)

#' The display metadata of a method
#'
#' @param method A method name in `SUPPORTED_METHOD_OUTPUTS`.
#' @return List: `label`, `color`, `order`, `blurb`.
#' @noRd
method_display <- function(method) {
  d <- SUPPORTED_METHOD_OUTPUTS[[method]]$display
  list(label = d$label, color = d$color, order = as.integer(d$order), blurb = d$blurb)
}

#' Schema columns of every table
#'
#' The column names of every table in `inst/sql/schema.sql`, kept as internal package
#' data (`R/sysdata.rda`, made by `data-raw/schema_columns.R`) so that reading them
#' costs nothing; reading them from the schema takes about 40 ms and they are needed
#' hundreds of times in an ingest. A test fails if the saved lists differ from
#' `read_schema_columns()`.
#'
#' @return Named list of character vectors (table -> columns, in schema order).
#' @noRd
schema_columns <- function() schema_columns_data

#' Read the column names of every table from the schema
#'
#' Creates the schema in memory. Used to make `schema_columns_data` and to test it.
#'
#' @return Named list of character vectors (table -> columns, in schema order).
#' @noRd
read_schema_columns <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (stmt in read_sql_statements(sql_path("schema.sql"))) DBI::dbExecute(con, stmt)
  tabs <- DBI::dbListTables(con)
  stats::setNames(
    lapply(tabs, function(t) DBI::dbGetQuery(con, sprintf("PRAGMA table_info(%s)", t))$name),
    tabs
  )
}

#' Columns of a results-output table
#'
#' The columns a results-output Parquet file must have: the schema table's
#' columns, minus its surrogate ids, plus the natural-key columns that replace
#' them (listed first). Columns a file may omit (optional contract columns, and
#' columns the database does not store) are not returned unless
#' `include_optional = TRUE`.
#'
#' @param table A table name (see [results_tables()]).
#' @param include_optional Also return the optional columns.
#' @return Character vector of column names.
#' @export
contract_columns <- function(table, include_optional = FALSE) {
  spec <- RESULT_TABLES[[table]]
  if (is.null(spec)) {
    cli::cli_abort("{.val {table}} is not a results-output table; see {.fn results_tables}.")
  }
  if (isFALSE(spec$loaded)) {
    return(spec$columns)
  }
  cols <- unique(c(spec$add, setdiff(schema_columns()[[table]], spec$drop)))
  optional <- unique(c(spec$optional, spec$extra))
  required <- setdiff(cols, optional)
  if (include_optional) unique(c(required, optional)) else required
}

#' The results-output tables
#'
#' @return A data frame with one row per table: its name, whether it is
#'   written per dataset (`"dataset"`) or once across datasets (`"cross"`),
#'   its artifact-path columns (`NA` if none), and whether the database loads
#'   it (`loaded`).
#' @export
results_tables <- function() {
  data.frame(
    table = names(RESULT_TABLES),
    scope = vapply(RESULT_TABLES, function(x) x$scope, character(1)),
    paths = vapply(RESULT_TABLES, function(x) x$paths[1] %||% NA_character_, character(1)),
    loaded = vapply(RESULT_TABLES, function(x) !isFALSE(x$loaded), logical(1)),
    row.names = NULL
  )
}

#' The methods whose output this package can load
#'
#' @return A data frame: method, supported `method_output_version`s.
#' @export
supported_methods <- function() {
  data.frame(
    method = names(SUPPORTED_METHOD_OUTPUTS),
    versions = vapply(
      SUPPORTED_METHOD_OUTPUTS,
      function(x) paste(x$versions, collapse = ","),
      character(1)
    ),
    row.names = NULL
  )
}
