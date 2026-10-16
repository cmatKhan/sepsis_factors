#' Load a results output into the stability database
#'
#' Validates the results ([validate_results()]), then loads each dataset
#' ([load_dataset()]) and the cross-dataset tables ([load_cross_dataset()]),
#' creating the database if it is new. The contract tables are taken as they
#' are in the results, with natural keys resolved to ids. After each load the
#' derive step ([derive_dataset()], [derive_cross_dataset()]) builds the
#' tables that let the app run from the database alone.
#'
#' @param results_root The results directory written by `sepsisfactors`.
#' @param db_path Path to the SQLite file.
#' @param datasets Dataset ids to load; `NULL` for all in the manifest.
#' @param artifact_mode `"link"`, `"copy"`, `"move"` or `"symlink"`; see
#'   [load_dataset()].
#' @return A list: `datasets` (one [load_dataset()] result per dataset),
#'   `cross_dataset` (the [load_cross_dataset()] result, or `NULL`) and
#'   `derived` (the rows and seconds of each derive step, per dataset, and
#'   `cross_dataset`).
#' @export
ingest_results <- function(
  results_root,
  db_path,
  datasets = NULL,
  artifact_mode = c("link", "copy", "move", "symlink")
) {
  artifact_mode <- match.arg(artifact_mode)
  manifest <- validate_results(results_root, datasets)
  # Datasets are loaded in a fixed order (C locale, by id), so ids come out the
  # same as in a parallel build, whose merge uses that order.
  ids <- sort(datasets %||% manifest$datasets, method = "radix")
  cli::cli_inform("Loading {length(ids)} dataset{?s} into {.file {db_path}}.")
  loaded <- lapply(
    stats::setNames(ids, ids),
    function(id) load_dataset(results_root, db_path, id, artifact_mode)
  )
  derived <- lapply(
    stats::setNames(ids, ids),
    function(id) derive_dataset(results_root, db_path, id)
  )
  cross <- load_cross_dataset(results_root, db_path, artifact_mode)
  derived$cross_dataset <- derive_cross_dataset(db_path)
  invisible(list(datasets = loaded, cross_dataset = cross, derived = derived))
}
