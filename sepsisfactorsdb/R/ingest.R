#' Load a results output into the stability database
#'
#' Validates the results ([validate_results()]), then loads each dataset
#' ([load_dataset()]) and the cross-dataset tables ([load_cross_dataset()]),
#' creating the database if it is new. Nothing is computed here: every table
#' is taken as it is in the results, with natural keys resolved to ids.
#'
#' @param results_root The results directory written by `sepsisfactors`.
#' @param db_path Path to the SQLite file.
#' @param datasets Dataset ids to load; `NULL` for all in the manifest.
#' @param artifact_mode `"link"`, `"copy"` or `"move"`; see [load_dataset()].
#' @return A list: `datasets` (one [load_dataset()] result per dataset) and
#'   `cross_dataset` (the [load_cross_dataset()] result, or `NULL`).
#' @export
ingest_results <- function(
  results_root,
  db_path,
  datasets = NULL,
  artifact_mode = c("link", "copy", "move")
) {
  artifact_mode <- match.arg(artifact_mode)
  manifest <- validate_results(results_root, datasets)
  ids <- datasets %||% manifest$datasets
  cli::cli_inform("Loading {length(ids)} dataset{?s} into {.file {db_path}}.")
  loaded <- lapply(
    stats::setNames(ids, ids),
    function(id) load_dataset(results_root, db_path, id, artifact_mode)
  )
  cross <- load_cross_dataset(results_root, db_path, artifact_mode)
  invisible(list(datasets = loaded, cross_dataset = cross))
}
