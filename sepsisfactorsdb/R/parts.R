# Part files of the parallel build (see ingest_parallel()). A part is a SQLite
# file with the full schema holding the rows of one unit of work; merge_parts()
# combines them. The load part of a dataset has everything load_dataset() writes
# plus the derived tables (or only the dataset-level ones when the dataset is
# split into chunks); a chunk part has the fit-level derived tables of a slice of
# the dataset's fits; a projection part has the projection values of one source
# dataset.

# The tables a chunk part keeps; the rest of its copy of the load part is pruned.
CHUNK_TABLES <- c(
  "factor_top_genes", "fit_scores", "fit_diag", "wgcna_dendrograms", "driver_genes",
  "fit_summary", "factor_association", "method_agreement"
)

#' Where a part file is built and kept
#'
#' A part is built on `work_dir` (node-local disk) and moved to `part_dir` when
#' it is complete, so a part in `part_dir` is always whole.
#'
#' @param name File name without extension.
#' @noRd
part_file <- function(dir, name) {
  file.path(
    dir,
    paste0(gsub("[^A-Za-z0-9_.-]", "_", name), ".sqlite")
  )
}

#' Close a part so that it is one self-contained file
#'
#' @param con An open connection to the part.
#' @noRd
seal_part <- function(con) {
  DBI::dbExecute(con, "PRAGMA wal_checkpoint(TRUNCATE)")
  DBI::dbExecute(con, "PRAGMA journal_mode = DELETE")
  invisible(NULL)
}

#' Move a finished file into place, never leaving a partial one
#'
#' @noRd
publish_file <- function(from, to) {
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(to, ".tmp", Sys.getpid())
  if (!file.copy(from, tmp, overwrite = TRUE)) {
    cli::cli_abort("Could not copy {.file {from}} to {.file {dirname(to)}}.")
  }
  if (!file.rename(tmp, to)) {
    unlink(tmp)
    cli::cli_abort("Could not move the part into {.file {to}}.")
  }
  unlink(from)
  invisible(to)
}

#' Plan the chunks of a dataset
#'
#' A dataset whose work (fits plus pattern-driver files) exceeds `chunk_fits`
#' has its fit-level derived tables made in several chunks. The representative
#' fits, where the expensive steps run, are cut into consecutive slices of the
#' order agreement rows are written in (`agreement_fits()`); the other fits are
#' spread over the chunks.
#'
#' @param results_root The results directory.
#' @param dataset_id Dataset id.
#' @param chunk_fits Work per chunk.
#' @return A list of character vectors of fit keys, or `NULL` when the dataset
#'   is one unit.
#' @noRd
plan_chunks <- function(results_root, dataset_id, chunk_fits) {
  tdir <- file.path(results_dataset_dir(results_root, dataset_id), "tables")
  fits <- read_result_table(
    file.path(tdir, "fits.parquet"),
    c("fit_key", "method", "rank", "representative", "status")
  )
  fits$fit_id <- seq_len(nrow(fits)) # load_dataset() numbers a new part's fits in file order
  drivers_file <- file.path(tdir, "pattern_drivers.parquet")
  n_drivers <- if (file.exists(drivers_file)) {
    nrow(read_result_table(drivers_file, "fit_key"))
  } else {
    0L
  }
  k <- ceiling((nrow(fits) + n_drivers) / chunk_fits)
  if (is.na(k) || k <= 1L) {
    return(NULL)
  }
  reps <- agreement_fits(fits)
  rest <- fits[!fits$fit_key %in% reps$fit_key, , drop = FALSE]
  slice <- function(x, k) split(x, cut(seq_along(x), breaks = k, labels = FALSE))
  k <- min(k, max(nrow(reps), 1L))
  a <- slice(reps$fit_key, k)
  b <- if (nrow(rest)) slice(rest$fit_key, k) else list()
  chunks <- lapply(seq_len(k), function(i) c(a[[as.character(i)]], b[[as.character(i)]]))
  unname(Filter(length, chunks))
}

#' Build the load part of a dataset
#'
#' Validates nothing (the pipeline does); runs [load_dataset()] on a fresh part
#' file and the derive step ([derive_dataset()]), all of it for a dataset that is
#' one unit, only the dataset-level tables for a chunked one.
#'
#' @param results_root The results directory.
#' @param dataset_id Dataset id.
#' @param part_dir Where finished parts are kept.
#' @param work_dir Where parts are built (node-local disk).
#' @param artifact_base Path of the final database (artifacts go beside it).
#' @param artifact_mode See [load_dataset()].
#' @param chunked Whether the fit-level tables are made by chunk parts.
#' @return The path of the part.
#' @keywords internal
#' @export
build_load_part <- function(
  results_root,
  dataset_id,
  part_dir,
  work_dir,
  artifact_base,
  artifact_mode,
  chunked
) {
  work_dir <- work_dir %||% tempdir()
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  work <- part_file(work_dir, paste0(dataset_id, "-load-", Sys.getpid()))
  unlink(c(work, paste0(work, c("-wal", "-shm"))))
  load_dataset(results_root, work, dataset_id, artifact_mode, artifact_base)
  derive_dataset(
    results_root,
    work,
    dataset_id,
    level = if (chunked) "dataset" else "all",
    artifact_base = artifact_base
  )
  con <- open_stability_db(work)
  seal_part(con)
  DBI::dbDisconnect(con)
  publish_file(work, part_file(part_dir, dataset_id))
}

#' Build a chunk part
#'
#' Works on a copy of the dataset's load part (it needs the fit and factor ids, the
#' genes and the metrics there), derives the fit-level tables of `fit_keys`, and
#' keeps only those tables. The rows refer to the load part's ids, which the
#' merge renumbers together with the load part's.
#'
#' @param load_part The dataset's load part.
#' @param fit_keys The fits of this chunk.
#' @param chunk Chunk number, for the file name.
#' @inheritParams build_load_part
#' @return The path of the part.
#' @keywords internal
#' @export
build_chunk_part <- function(
  results_root,
  dataset_id,
  load_part,
  fit_keys,
  chunk,
  part_dir,
  work_dir,
  artifact_base
) {
  work_dir <- work_dir %||% tempdir()
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  work <- part_file(work_dir, sprintf("%s-chunk%03d-%d", dataset_id, chunk, Sys.getpid()))
  unlink(c(work, paste0(work, c("-wal", "-shm"))))
  file.copy(load_part, work)
  derive_dataset(
    results_root,
    work,
    dataset_id,
    level = "fits",
    fit_keys = fit_keys,
    artifact_base = artifact_base
  )
  con <- open_stability_db(work)
  DBI::dbExecute(con, "PRAGMA foreign_keys = OFF")
  tabs <- DBI::dbGetQuery(
    con,
    "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%'"
  )$name
  for (t in setdiff(tabs, CHUNK_TABLES)) DBI::dbExecute(con, sprintf("DELETE FROM %s", t))
  seal_part(con)
  DBI::dbExecute(con, "VACUUM")
  DBI::dbDisconnect(con)
  publish_file(work, part_file(part_dir, sprintf("%s-chunk%03d", dataset_id, chunk)))
}

#' Build the projection-value part of one source dataset
#'
#' @param projections The projections of the source dataset: `projection_id` (as
#'   in the merged database) and `path`.
#' @param source_id The source dataset, for the file name.
#' @inheritParams build_load_part
#' @return The path of the part.
#' @keywords internal
#' @export
build_projection_part <- function(projections, source_id, part_dir, work_dir, artifact_base) {
  work_dir <- work_dir %||% tempdir()
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  work <- part_file(work_dir, paste0("projections-", source_id, "-", Sys.getpid()))
  unlink(c(work, paste0(work, c("-wal", "-shm"))))
  derive_cross_dataset(
    artifact_base,
    projections = projections,
    out_path = work,
    artifact_base = artifact_base
  )
  con <- open_stability_db(work)
  seal_part(con)
  DBI::dbDisconnect(con)
  publish_file(work, part_file(part_dir, paste0("projections-", source_id)))
}
