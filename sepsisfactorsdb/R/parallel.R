# The parallel build: targets + crew. See ingest_parallel().

#' Targets for the parallel build
#'
#' Builds the database from part files made in parallel and merged. The targets,
#' per dataset `<id>` (datasets sorted by id, the order of the merge):
#'
#' * `results_<id>`: file target over the dataset's `tables/` directory.
#' * `part_<id>`: validates and loads the dataset into its own part file with its
#'   derived tables (`build_load_part()`).
#' * `chunk_<id>_<k>`: for a dataset above `chunk_fits` of work (fits plus
#'   pattern-driver files), the fit-level derived tables of one slice of its
#'   fits, each in its own part file.
#' * `merged`: merges every part ([merge_parts()]) and runs the cross-dataset load
#'   ([load_cross_dataset()]), in the main process.
#' * `projection_index`, `proj_<id>`: the projection values of each source
#'   dataset, in parallel, as part files.
#' * `database`: adds the projection parts, checks and publishes the final
#'   database (a file target on `db_path`).
#'
#' Each part is a target, so a rerun rebuilds only the parts of datasets whose
#' results changed, then merges again. Deleting the database makes `database`
#' outdated (a file target whose file is gone is rebuilt), so no
#' `tar_invalidate()` is needed. A part that fails leaves the old database in
#' place: the merge works in `merge_dir` and the final copy is an atomic rename.
#' Targets that run a dataset's load use the controller `"default"`, chunks of a
#' large dataset `"big"` (see [ingest_controller()]); the merge steps run in the
#' main process.
#'
#' @param config An ingest configuration: [read_ingest_config()], or a list with
#'   `results_root` and `db_path` (the rest as [ingest_defaults()]).
#' @return A list of targets for `_targets.R`.
#' @export
ingest_parallel_targets <- function(config) {
  config <- utils::modifyList(ingest_defaults(), config)
  root <- config$results_root
  db_path <- config$db_path
  ids <- sort(config$datasets %||% read_manifest(root)$datasets, method = "radix")
  nm <- function(prefix, id) paste0(prefix, "_", gsub("[^A-Za-z0-9_]", "_", id))
  main <- function(name, command, ...) targets::tar_target_raw(name, command, deployment = "main", ...)
  on <- function(name, command, controller, ...) {
    targets::tar_target_raw(
      name,
      command,
      resources = targets::tar_resources(crew = targets::tar_resources_crew(controller = controller)),
      ...
    )
  }
  part_dir <- config$part_dir
  work_dir <- config$work_dir
  mode <- config$artifact_mode

  manifest <- main("results_manifest", substitute(file.path(ROOT, "manifest.json"), list(ROOT = root)), format = "file")
  cross <- main(
    "results_cross_dataset",
    substitute(if (dir.exists(DIR)) DIR else character(), list(DIR = file.path(root, "cross_dataset"))),
    format = "file"
  )
  targets_list <- list(manifest, cross)
  part_names <- character()
  units <- integer()
  for (i in seq_along(ids)) {
    id <- ids[i]
    chunks <- plan_chunks(root, id, config$chunk_fits)
    controller <- if (is.null(chunks)) "default" else "big"
    results_name <- nm("results", id)
    part_name <- nm("part", id)
    targets_list <- c(
      targets_list,
      list(
        main(
          results_name,
          substitute(file.path(ROOT, "datasets", ID, "tables"), list(ROOT = root, ID = id)),
          format = "file"
        ),
        on(
          part_name,
          substitute(
            {
              results_manifest
              RESULTS
              sepsisfactorsdb::validate_results(ROOT, ID)
              sepsisfactorsdb::build_load_part(ROOT, ID, PART_DIR, WORK_DIR, DB, MODE, CHUNKED)
            },
            list(
              RESULTS = as.name(results_name), ROOT = root, ID = id, PART_DIR = part_dir,
              WORK_DIR = work_dir, DB = db_path, MODE = mode, CHUNKED = !is.null(chunks)
            )
          ),
          controller,
          format = "file"
        )
      )
    )
    part_names <- c(part_names, part_name)
    units <- c(units, i)
    for (k in seq_along(chunks)) {
      chunk_name <- nm(paste0("chunk_", k), id)
      targets_list <- c(
        targets_list,
        list(on(
          chunk_name,
          substitute(
            sepsisfactorsdb::build_chunk_part(ROOT, ID, PART, KEYS, K, PART_DIR, WORK_DIR, DB),
            list(
              ROOT = root, ID = id, PART = as.name(part_name), KEYS = chunks[[k]], K = k,
              PART_DIR = part_dir, WORK_DIR = work_dir, DB = db_path
            )
          ),
          "big",
          format = "file"
        ))
      )
      part_names <- c(part_names, chunk_name)
      units <- c(units, i)
    }
  }
  merged <- main(
    "merged",
    substitute(
      {
        results_cross_dataset
        sepsisfactorsdb::merge_dataset_parts(PARTS, UNITS, ROOT, DB, MODE, MERGE_DIR)
      },
      list(
        PARTS = as.call(c(as.name("c"), lapply(part_names, as.name))),
        UNITS = units, ROOT = root, DB = db_path, MODE = mode, MERGE_DIR = config$merge_dir
      )
    ),
    format = "file"
  )
  index <- main("projection_index", quote(sepsisfactorsdb::projection_index(merged)))
  sources <- projection_sources(root, ids)
  proj_names <- nm("proj", sources)
  proj <- lapply(seq_along(sources), function(i) {
    on(
      proj_names[i],
      substitute(
        sepsisfactorsdb::build_projection_part(
          projection_index[projection_index$source == ID, c("projection_id", "path"), drop = FALSE],
          ID, PART_DIR, WORK_DIR, DB
        ),
        list(ID = sources[i], PART_DIR = part_dir, WORK_DIR = work_dir, DB = db_path)
      ),
      "default",
      format = "file"
    )
  })
  database <- main(
    "database",
    substitute(
      sepsisfactorsdb::finish_database(merged, PROJ, DB, MERGE_DIR),
      list(
        PROJ = as.call(c(as.name("c"), lapply(proj_names, as.name))),
        DB = db_path, MERGE_DIR = config$merge_dir
      )
    ),
    format = "file"
  )
  c(targets_list, list(merged, index), proj, list(database))
}

#' Merge the dataset parts and load the cross-dataset tables
#'
#' The `merged` target.
#'
#' @return The merged file's path (in `merge_dir`).
#' @keywords internal
#' @export
merge_dataset_parts <- function(parts, units, results_root, db_path, artifact_mode, merge_dir) {
  merge_dir <- merge_dir %||% file.path(tempdir(), "merge")
  merged <- merge_parts(unname(parts), merge_dir = merge_dir, units = units)
  load_cross_dataset(results_root, merged, artifact_mode, artifact_base = db_path)
  con <- open_stability_db(merged)
  seal_part(con)
  DBI::dbDisconnect(con)
  merged
}

#' The projections of the merged database and the dataset each comes from
#'
#' @keywords internal
#' @export
projection_index <- function(merged) {
  # read-only and without pragmas, so the file target `merged` keeps its hash
  con <- DBI::dbConnect(RSQLite::SQLite(), merged, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT p.projection_id, p.path, f.dataset_id AS source FROM projections p
     JOIN fits f ON f.fit_id = p.source_fit_id ORDER BY p.projection_id"
  )
}

#' The datasets whose fits are the source of a projection
#'
#' From the results tables, at pipeline definition time.
#'
#' @noRd
projection_sources <- function(results_root, ids) {
  file <- file.path(results_cross_dir(results_root), "tables", "projections.parquet")
  if (!file.exists(file)) {
    return(character())
  }
  keys <- unique(read_result_table(file, "source_fit_key")$source_fit_key)
  sources <- vapply(ids, function(id) {
    f <- file.path(results_dataset_dir(results_root, id), "tables", "fits.parquet")
    any(read_result_table(f, "fit_key")$fit_key %in% keys)
  }, logical(1))
  ids[sources]
}

#' Add the projection parts to the merged database, check it and publish it
#'
#' The `database` target. Works on a copy of `merged`, so `merged` stays as the
#' target recorded it.
#'
#' @return `db_path`.
#' @keywords internal
#' @export
finish_database <- function(merged, projection_parts, db_path, merge_dir) {
  merge_dir <- merge_dir %||% file.path(tempdir(), "merge")
  work <- file.path(merge_dir, "final.sqlite")
  unlink(c(work, paste0(work, c("-wal", "-shm"))))
  file.copy(merged, work)
  if (length(projection_parts)) {
    merge_parts(unname(projection_parts), into = work, renumber = FALSE)
  }
  publish_database(work, db_path)
  unlink(work)
  db_path
}

#' Build the database in parallel
#'
#' Runs the pipeline of [ingest_parallel_targets()] with `targets::tar_make()`:
#' every dataset is loaded into its own part file by a crew worker (local
#' processes or SLURM jobs, whichever `controller` is), the parts are merged
#' ([merge_parts()]) and the result replaces `db_path`. The result is the same
#' database, ids included, that [ingest_results()] builds serially.
#'
#' Large datasets (above `chunk_fits` of work: fits plus pattern-driver files)
#' have their fit-level derived tables made in several chunks that run at once.
#' Parts are built on `work_dir` (node-local disk) and kept in `part_dir`, which
#' must be visible to the main process, so on a cluster it is a shared directory.
#' With a given `store`, a rerun reuses every part whose dataset results did not
#' change; without one, the store is temporary and everything is built.
#'
#' Targets runs the pipeline in this R session (`callr_function = NULL`), because
#' the controller is an object of this session. For a run that outlives it, use
#' [use_ingest()], whose `_targets.R` builds the controller from `ingest.yml`.
#'
#' @param results_root The results directory.
#' @param db_path Path of the database to write (replaced only on success).
#' @param controller An [ingest_controller()].
#' @param datasets Dataset ids; `NULL` for all in the manifest.
#' @param artifact_mode `"link"`, `"copy"`, `"move"` or `"symlink"`; see
#'   [load_dataset()].
#' @param part_dir Where finished parts are kept. Default: `parts/` in `store`.
#' @param work_dir Where parts are built, on the worker (default: its temporary
#'   directory).
#' @param merge_dir Where the merge works (default: a temporary directory).
#' @param chunk_fits Work per chunk; see above.
#' @param store The targets store. `NULL`: a temporary one.
#' @return A data frame of the targets and their progress (`"completed"` for what
#'   was built, `"skipped"` for what was reused), invisibly.
#' @export
ingest_parallel <- function(
  results_root,
  db_path,
  controller = ingest_controller("local"),
  datasets = NULL,
  artifact_mode = c("link", "copy", "move", "symlink"),
  part_dir = NULL,
  work_dir = NULL,
  merge_dir = NULL,
  chunk_fits = 400L,
  store = NULL
) {
  artifact_mode <- match.arg(artifact_mode)
  store <- store %||% tempfile("ingest-store-")
  config <- list(
    results_root = normalizePath(results_root),
    db_path = normalizePath(db_path, mustWork = FALSE),
    datasets = datasets,
    artifact_mode = artifact_mode,
    part_dir = part_dir %||% file.path(normalizePath(store, mustWork = FALSE), "parts"),
    work_dir = work_dir,
    merge_dir = merge_dir,
    chunk_fits = chunk_fits
  )
  options(sepsisfactorsdb.parallel = list(config = config, controller = controller))
  on.exit(options(sepsisfactorsdb.parallel = NULL), add = TRUE)
  script <- tempfile("ingest-", fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  writeLines(
    c(
      "library(targets)",
      "targets::tar_option_set(packages = character(), error = 'stop')",
      "sepsisfactorsdb::parallel_pipeline()"
    ),
    script
  )
  cli::cli_inform("Building {.file {db_path}} in parallel with the {.val {controller$controllers[[1]]$launcher$name}} controllers.")
  targets::tar_make(script = script, store = store, callr_function = NULL, reporter = "silent")
  invisible(targets::tar_progress(store = store))
}

#' The pipeline of [ingest_parallel()], read from the session's options
#'
#' Called from the generated `_targets.R`.
#'
#' @keywords internal
#' @export
parallel_pipeline <- function() {
  opts <- getOption("sepsisfactorsdb.parallel")
  targets::tar_option_set(controller = opts$controller, storage = "main", retrieval = "main")
  ingest_parallel_targets(opts$config)
}
