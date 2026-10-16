# crew controllers for the parallel build: where the work runs is one option.
# Follows the idioms of sepsisfactors' R/targets_controllers.R (one controller per
# resource profile, combined with crew_controller_group() and chosen per target
# with tar_resources_crew(controller = "<name>"); SLURM workers get R from
# `setup_lines` and write logs to a directory) without importing that package.

#' Read an ingest.yml
#'
#' The settings of a parallel build: paths, the controller and its resources.
#' [use_ingest()] writes a commented template.
#'
#' @param path Path to the file.
#' @return A list. Missing settings take the defaults of [ingest_defaults()].
#' @export
read_ingest_config <- function(path = "ingest.yml") {
  if (!file.exists(path)) {
    cli::cli_abort("No {.file {path}}; {.fn use_ingest} writes one.")
  }
  cfg <- yaml::read_yaml(path)
  cfg <- utils::modifyList(ingest_defaults(), cfg)
  for (key in c("results_root", "db_path")) {
    if (is.null(cfg[[key]])) cli::cli_abort("{.file {path}} needs {.field {key}}.")
  }
  cfg
}

#' The defaults of an ingest configuration
#'
#' @return A list of the settings [read_ingest_config()] fills in, as `ingest.yml`
#'   documents them.
#' @export
ingest_defaults <- function() {
  list(
    results_root = NULL,
    db_path = NULL,
    datasets = NULL,
    artifact_mode = "link",
    part_dir = "ingest_parts",
    work_dir = NULL,
    merge_dir = NULL,
    chunk_fits = 400L,
    controller = list(
      type = "local",
      workers = NULL,
      seconds_idle = 30,
      slurm = list(
        partition = NULL,
        cpus_per_task = 1L,
        memory_gigabytes_required = 8,
        time_minutes = 240,
        setup_lines = list(),
        log_dir = "logs/crew",
        workers = 200L,
        big = list(memory_gigabytes_required = 32, cpus_per_task = 2L, time_minutes = 480, workers = 20L)
      )
    )
  )
}

#' The crew controllers of a parallel build
#'
#' Returns a controller group with two controllers, `"default"` (one dataset's
#' load, or a small chunk) and `"big"` (a chunk of a large dataset), which
#' [ingest_parallel_targets()] selects per target. `type = "local"` runs them as
#' local processes with [crew::crew_controller_local()]; `type = "slurm"` as SLURM
#' jobs with [crew.cluster::crew_controller_slurm()]. Building the controller
#' submits nothing; jobs are submitted when targets starts the pipeline.
#'
#' The merge and the cross-dataset steps always run in the main process, which
#' therefore needs the memory for them (the sbatch script of [use_ingest()]
#' asks for it).
#'
#' @param type `"local"` or `"slurm"`.
#' @param workers Maximum number of simultaneous workers of the `"default"`
#'   controller. `NULL`: locally `max(1, parallel::detectCores() - 2)`; on SLURM
#'   the `workers` of the config. The `"big"` controller gets half (locally) or the
#'   config's `big$workers` (SLURM).
#' @param config An ingest configuration ([read_ingest_config()]); its
#'   `controller` block gives the defaults of the other arguments.
#' @param ... Overrides for the SLURM resources: `partition`, `cpus_per_task`,
#'   `memory_gigabytes_required`, `time_minutes`, `setup_lines`, `log_dir`, and
#'   `big` (a list of the same for the big controller).
#' @return A [crew::crew_controller_group()].
#' @export
ingest_controller <- function(type = c("local", "slurm"), workers = NULL, config = ingest_defaults(), ...) {
  type <- match.arg(type)
  ctl <- config$controller
  seconds_idle <- ctl$seconds_idle %||% 30
  if (type == "local") {
    n <- workers %||% ctl$workers %||% max(1L, parallel::detectCores() - 2L)
    return(crew::crew_controller_group(
      crew::crew_controller_local(name = "default", workers = n, seconds_idle = seconds_idle),
      crew::crew_controller_local(name = "big", workers = max(1L, n %/% 2L), seconds_idle = seconds_idle)
    ))
  }
  spec <- override(override(ingest_defaults()$controller$slurm, ctl$slurm), list(...))
  big <- override(spec[setdiff(names(spec), c("big", "workers"))], spec$big)
  log_dir <- spec$log_dir
  dir.create(file.path(log_dir, "scripts"), recursive = TRUE, showWarnings = FALSE)
  script_lines <- c(unlist(spec$setup_lines), sprintf('cd "%s"', getwd()))
  slurm <- function(name, s, n) {
    crew.cluster::crew_controller_slurm(
      name = name,
      workers = n,
      seconds_idle = seconds_idle,
      options_cluster = crew.cluster::crew_options_slurm(
        script_lines = script_lines,
        script_directory = file.path(log_dir, "scripts"),
        log_output = file.path(log_dir, "%x_%j.out"),
        log_error = NULL,
        memory_gigabytes_required = s$memory_gigabytes_required,
        cpus_per_task = s$cpus_per_task,
        time_minutes = s$time_minutes,
        partition = s$partition
      )
    )
  }
  crew::crew_controller_group(
    slurm("default", spec, workers %||% ctl$workers %||% spec$workers),
    slurm("big", big, big$workers %||% 20L)
  )
}

#' Replace settings by name
#'
#' Unlike [utils::modifyList()] it replaces a list-valued setting (such as
#' `setup_lines`) outright instead of merging it by position.
#'
#' @noRd
override <- function(base, new) {
  for (nm in names(new)) base[nm] <- list(new[[nm]])
  base
}
