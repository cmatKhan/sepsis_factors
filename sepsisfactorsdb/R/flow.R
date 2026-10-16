#' Targets for the ingest workflow
#'
#' One file target per dataset over its results directory, one `db_<dataset>`
#' target that loads it ([load_dataset()]), and `db_cross_dataset`
#' ([load_cross_dataset()]) after all of them; each load is followed by its
#' derive step ([derive_dataset()], [derive_cross_dataset()]), so a re-run loads only the
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
#' @param artifact_mode `"link"`, `"copy"`, `"move"` or `"symlink"`; see
#'   [load_dataset()].
#' @return A list of targets.
#' @export
ingest_targets <- function(
  results_root,
  db_path,
  datasets = NULL,
  artifact_mode = c("link", "copy", "move", "symlink")
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
          sepsisfactorsdb::derive_dataset(ROOT, DB, ID)
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
        sepsisfactorsdb::derive_cross_dataset(DB)
      },
      list(DEPS = deps, ROOT = results_root, DB = db_path, MODE = artifact_mode)
    )
  )
  c(list(manifest), dataset_files, list(cross_files), loads, list(cross))
}

#' Set up an ingest workflow in a directory
#'
#' By default writes the files of the parallel build ([ingest_parallel()]) next
#' to each other: a `_targets.R` that builds the controller from `ingest.yml`
#' ([read_ingest_config()], [ingest_controller()]) and runs
#' [ingest_parallel_targets()], the commented `ingest.yml`, and
#' `run_ingest.sbatch`, which runs the main process as a SLURM job. Edit
#' `ingest.yml`, then run `targets::tar_make()` (or `sbatch run_ingest.sbatch`).
#' With `serial = TRUE` it writes only the `_targets.R` of the serial
#' workflow ([ingest_targets()]).
#'
#' @param path Directory (created if needed).
#' @param serial Write the serial workflow instead.
#' @return The path of the `_targets.R` file, invisibly.
#' @export
use_ingest <- function(path = ".", serial = FALSE) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  files <- if (serial) c("_targets.R" = "_targets_serial.R") else c("_targets.R" = "_targets.R", "ingest.yml" = "ingest.yml", "run_ingest.sbatch" = "run_ingest.sbatch")
  existing <- file.path(path, names(files))[file.exists(file.path(path, names(files)))]
  if (length(existing)) {
    cli::cli_abort("{.file {existing}} already exist{?s}; not overwriting {?it/them}.")
  }
  for (i in seq_along(files)) {
    file.copy(system.file("templates", files[[i]], package = "sepsisfactorsdb"), file.path(path, names(files)[i]))
  }
  Sys.chmod(file.path(path, "run_ingest.sbatch")[file.exists(file.path(path, "run_ingest.sbatch"))], "755")
  target <- file.path(path, "_targets.R")
  cli::cli_inform("Wrote {.file {target}}{if (!serial) ', ingest.yml and run_ingest.sbatch'}; edit the paths, then run {.code targets::tar_make()}.")
  invisible(target)
}
