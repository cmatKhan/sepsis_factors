#' Targets for the ingest workflow
#'
#' One file target per dataset over its results directory, one `db_<dataset>`
#' target that loads it ([load_dataset()]), and `db_cross_dataset`
#' ([load_cross_dataset()]) after all of them, so a re-run loads only the
#' datasets whose results changed. Each load validates its dataset first
#' ([validate_results()]). Use it in `_targets.R`, as [use_ingest()] sets up.
#'
#' Loads run in the main process, one after another, since they share one
#' SQLite file. If you delete the database file, run
#' `targets::tar_invalidate(starts_with("db_"))` before `tar_make()`.
#'
#' @param results_root The results directory written by `sepsisfactors`.
#' @param db_path Path to the SQLite file.
#' @param datasets Dataset ids; `NULL` for all in the manifest.
#' @param artifact_mode `"link"`, `"copy"` or `"move"`; see [load_dataset()].
#' @return A list of targets.
#' @export
ingest_targets <- function(
  results_root,
  db_path,
  datasets = NULL,
  artifact_mode = c("link", "copy", "move")
) {
  artifact_mode <- match.arg(artifact_mode)
  ids <- datasets %||% read_manifest(results_root)$datasets
  nm <- function(prefix, id) paste0(prefix, "_", gsub("[^A-Za-z0-9_]", "_", id))
  main <- function(name, command, ...) {
    targets::tar_target_raw(name, command, deployment = "main", ...)
  }

  manifest <- main(
    "results_manifest",
    substitute(file.path(ROOT, "manifest.json"), list(ROOT = results_root)),
    format = "file"
  )
  dataset_files <- lapply(ids, function(id) {
    main(
      nm("results", id),
      # The tables only: hashing every artifact file each run would be slow,
      # and a dataset's artifacts do not change without its tables changing.
      substitute(file.path(ROOT, "datasets", ID, "tables"), list(ROOT = results_root, ID = id)),
      format = "file"
    )
  })
  cross_files <- main(
    "results_cross_dataset",
    substitute(
      if (dir.exists(DIR)) DIR else character(),
      list(DIR = file.path(results_root, "cross_dataset"))
    ),
    format = "file"
  )
  loads <- lapply(ids, function(id) {
    main(
      nm("db", id),
      substitute(
        {
          results_manifest
          FILES
          sepsisfactorsdb::validate_results(ROOT, ID)
          sepsisfactorsdb::load_dataset(ROOT, DB, ID, MODE)
        },
        list(
          FILES = as.name(nm("results", id)),
          ROOT = results_root,
          DB = db_path,
          ID = id,
          MODE = artifact_mode
        )
      )
    )
  })
  deps <- as.call(c(as.name("{"), lapply(ids, function(id) as.name(nm("db", id)))))
  cross <- main(
    "db_cross_dataset",
    substitute(
      {
        DEPS
        results_cross_dataset
        sepsisfactorsdb::validate_results(ROOT, character())
        sepsisfactorsdb::load_cross_dataset(ROOT, DB, MODE)
      },
      list(DEPS = deps, ROOT = results_root, DB = db_path, MODE = artifact_mode)
    )
  )
  c(list(manifest), dataset_files, list(cross_files), loads, list(cross))
}

#' Set up an ingest workflow in a directory
#'
#' Writes a `_targets.R` that runs [ingest_targets()]. Edit the paths at its
#' top, then run `targets::tar_make()`.
#'
#' @param path Directory (created if needed).
#' @return The path of the `_targets.R` file, invisibly.
#' @export
use_ingest <- function(path = ".") {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  target <- file.path(path, "_targets.R")
  if (file.exists(target)) {
    cli::cli_abort("{.file {target}} already exists; not overwriting it.")
  }
  file.copy(system.file("templates", "_targets.R", package = "sepsisfactorsdb"), target)
  cli::cli_inform("Wrote {.file {target}}; edit the paths at its top, then run {.code targets::tar_make()}.")
  invisible(target)
}
