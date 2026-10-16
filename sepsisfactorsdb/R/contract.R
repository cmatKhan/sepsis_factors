# The results-output contract ("contract A"). One Parquet file per schema table;
# the columns are the schema table's columns with each surrogate id replaced by
# its natural key and the load-time timestamps optional. RESULT_TABLES says how
# each table differs from the schema; contract_columns() applies it to the
# schema, so the contract cannot drift from schema.sql.
#
# For each table:
#   scope  "dataset" (datasets/<id>/tables/) or "cross" (cross_dataset/tables/)
#   drop   schema columns that are surrogate ids, absent from the results
#   add    natural-key columns present in the results instead (placed first)
#   fill   columns the loader fills with the load time when the file lacks them
#   paths  columns holding an artifact file name (relative to the artifacts/
#          directory next to tables/); the loader stores
#          stability_artifacts/<dataset_id>/<file name>
RESULT_TABLES <- list(
  datasets = list(scope = "dataset", fill = "written_at"),
  methods = list(scope = "dataset"),
  dataset_artifacts = list(scope = "dataset", paths = "path"),
  matrix_diagnostics = list(scope = "dataset", fill = "computed_at"),
  fits = list(
    scope = "dataset",
    drop = "fit_id",
    add = "fit_key",
    fill = "written_at"
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
    add = c("fit_a_key", "fit_b_key")
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
    add = c("fit_a_key", "fit_b_key")
  ),
  similarity_histograms = list(scope = "dataset"),
  gene_modules = list(scope = "dataset", drop = "fit_id", add = "fit_key"),
  module_kme_summary = list(scope = "dataset", drop = "fit_id", add = "fit_key"),
  module_kme_histograms = list(scope = "dataset", drop = "fit_id", add = "fit_key"),
  wgcna_sft = list(scope = "dataset"),
  gene_significance = list(scope = "dataset"),
  icasso_clusters = list(
    scope = "dataset",
    drop = "centrotype_factor_id",
    add = c("centrotype_fit_key", "centrotype_factor_index"),
    paths = "dendro_file"
  ),
  icasso_membership = list(
    scope = "dataset",
    drop = "factor_id",
    add = c("fit_key", "factor_index")
  ),
  enrichment_queries = list(
    scope = "dataset",
    drop = c("query_id", "factor_id"),
    add = c("fit_key", "factor_index"),
    fill = "queried_at"
  ),
  enrichment_results = list(
    scope = "dataset",
    drop = "query_id",
    add = c("fit_key", "factor_index", "query_type", "direction")
  ),
  pattern_drivers = list(
    scope = "dataset",
    drop = c("driver_id", "factor_id"),
    add = c("fit_key", "factor_index"),
    fill = "computed_at",
    paths = "path"
  ),
  fit_redundancy = list(scope = "dataset", drop = "fit_id", add = "fit_key"),
  dataset_design = list(scope = "dataset"),
  samples = list(scope = "dataset"),
  variance_summary = list(scope = "dataset", fill = "fitted_at"),
  variance_terms = list(scope = "dataset"),
  variance_categories = list(scope = "dataset"),
  variance_bins = list(scope = "dataset"),
  trait_variance = list(scope = "dataset"),
  composition_correlations = list(scope = "dataset"),
  de_contrasts = list(scope = "dataset"),
  factor_variance_models = list(scope = "dataset"),
  factor_variance = list(
    scope = "dataset",
    drop = "factor_id",
    add = c("fit_key", "factor_index")
  ),
  variance_time_genes = list(scope = "dataset"),
  projections = list(
    scope = "cross",
    drop = c("projection_id", "source_fit_id"),
    add = "source_fit_key",
    paths = "path"
  ),
  gene_space_agreement = list(
    scope = "cross",
    drop = c("source_fit_id", "target_fit_id"),
    add = c("source_fit_key", "target_fit_key")
  ),
  time_gene_recurrence = list(scope = "cross")
)

# What each supported method's output must look like, per method_output_version.
# The loader checks a results tree against this before writing anything: the
# `methods` row must agree, every ok fit must have the required artifacts, and
# the required tables must exist. `scores_unless_bootstrap` lists methods whose
# bootstrap fits carry no scores. A new method, or a changed output of an
# existing one, adds an entry or a version here.
SUPPORTED_METHOD_OUTPUTS <- list(
  pca = list(
    versions = 1L,
    family = "seed_sweep",
    sign_ambiguous = TRUE,
    has_loadings = TRUE,
    fit_artifacts = c("loadings", "scores", "diag"),
    tables = character()
  ),
  spca = list(
    versions = 1L,
    family = "param_grid",
    sign_ambiguous = TRUE,
    has_loadings = TRUE,
    fit_artifacts = c("loadings", "scores", "diag"),
    tables = character()
  ),
  ica = list(
    versions = 1L,
    family = "seed_sweep",
    sign_ambiguous = TRUE,
    has_loadings = TRUE,
    fit_artifacts = c("loadings", "scores", "diag"),
    scores_unless_bootstrap = TRUE,
    tables = character()
  ),
  nmf = list(
    versions = 1L,
    family = "seed_sweep",
    sign_ambiguous = FALSE,
    has_loadings = TRUE,
    fit_artifacts = c("loadings", "scores", "diag"),
    tables = character()
  ),
  cogaps = list(
    versions = 1L,
    family = "seed_sweep",
    sign_ambiguous = FALSE,
    has_loadings = TRUE,
    fit_artifacts = c("loadings", "scores", "diag"),
    tables = character()
  ),
  wgcna = list(
    versions = 1L,
    family = "param_grid",
    sign_ambiguous = FALSE,
    has_loadings = FALSE,
    fit_artifacts = c("scores", "diag"),
    tables = "gene_modules"
  )
)

#' Schema columns of every table
#'
#' Reads the column names from the schema by creating it in memory once.
#'
#' @return Named list of character vectors (table -> columns, in schema order).
#' @noRd
schema_columns <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      for (stmt in read_sql_statements(sql_path("schema.sql"))) DBI::dbExecute(con, stmt)
      tabs <- DBI::dbListTables(con)
      cache <<- stats::setNames(
        lapply(tabs, function(t) DBI::dbGetQuery(con, sprintf("PRAGMA table_info(%s)", t))$name),
        tabs
      )
    }
    cache
  }
})

#' Columns of a results-output table
#'
#' The columns a results-output Parquet file must have: the schema table's
#' columns, minus its surrogate ids, plus the natural-key columns that replace
#' them (listed first). Columns the loader fills with the load time
#' (`written_at`, `computed_at`, `queried_at`, `fitted_at`) are optional and
#' not returned unless `include_optional = TRUE`.
#'
#' @param table A table name (see [results_tables()]).
#' @param include_optional Also return the loader-filled columns.
#' @return Character vector of column names.
#' @export
contract_columns <- function(table, include_optional = FALSE) {
  spec <- RESULT_TABLES[[table]]
  if (is.null(spec)) {
    cli::cli_abort("{.val {table}} is not a results-output table; see {.fn results_tables}.")
  }
  cols <- schema_columns()[[table]]
  cols <- c(spec$add, setdiff(cols, spec$drop))
  if (!include_optional) cols <- setdiff(cols, spec$fill)
  cols
}

#' The results-output tables
#'
#' @return A data frame with one row per table: its name, whether it is
#'   written per dataset (`"dataset"`) or once across datasets (`"cross"`),
#'   and its artifact-path columns (`NA` if none).
#' @export
results_tables <- function() {
  data.frame(
    table = names(RESULT_TABLES),
    scope = vapply(RESULT_TABLES, function(x) x$scope, character(1)),
    paths = vapply(RESULT_TABLES, function(x) x$paths[1] %||% NA_character_, character(1)),
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
