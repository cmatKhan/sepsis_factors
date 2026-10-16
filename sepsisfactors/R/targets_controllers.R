# crew controllers for the targets pipeline: one crew_controller_slurm()
# per entry in config/pipeline.yml's `controllers:`, combined with
# crew_controller_group() and selected per target via
# tar_resources_crew(controller = "<name>") -- the "heterogeneous workers"
# pattern from the targets manual's crew chapter.

#' Read a project's pipeline.yml
#'
#' Besides the file's own settings (the datasets to run, `skip_methods`, the
#' cluster block, one controller profile per method), this adds what the
#' project directory holds or the environment provides: `dataset_configs`
#' (every available dataset's `config.yml`; see [discover_datasets()]),
#' `data_root` (see [default_data_root()]), `results_root` (default
#' `results`), and `files`, the project's `dataset_families.yml` and
#' `composition_markers.yml` if it has them, else the package's.
#'
#' @param path Path to the pipeline config.
#' @return The parsed config, with the additions above.
#' @export
read_pipeline_config <- function(path = "pipeline.yml") {
  pipeline <- yaml::read_yaml(path)
  if (length(pipeline$datasets) == 0) {
    cli::cli_abort("{.file {path}} needs a {.field datasets} list.")
  }
  if (length(pipeline$controllers) == 0) {
    cli::cli_abort("{.file {path}} needs a {.field controllers} block.")
  }
  dir <- dirname(path)
  pipeline$dataset_configs <- discover_datasets(file.path(dir, "datasets"))
  pipeline$data_root <- default_data_root(path)
  if (is.null(pipeline$data_root)) {
    cli::cli_abort(c(
      "No data root.",
      "i" = "Set {.field data_root} in {.file {path}}, the {.envvar SEPSIS_DATA_ROOT} variable or the {.code sepsisfactors.data_root} option."
    ))
  }
  pipeline$results_root <- pipeline$results_root %||% "results"
  config_file <- function(name) {
    project <- file.path(dir, name)
    if (file.exists(project)) project else system.file("config", name, package = "sepsisfactors")
  }
  pipeline$files <- list(
    dataset_families = config_file("dataset_families.yml"),
    composition_markers = config_file("composition_markers.yml")
  )
  pipeline
}

#' Shell lines a worker's SLURM script runs before starting R
#'
#' crew.cluster's `script_lines` hook (its docs' `module load R` example): the
#' config's `cluster.setup_lines` (whatever puts R on the path on the cluster:
#' `module load`, a spack activation, ...), then `cd` to the project so R
#' starts with the project's settings.
#'
#' @param cluster The pipeline config's `cluster:` block.
#' @param project_root Project directory.
#' @return Character vector of shell lines.
#' @noRd
worker_script_lines <- function(cluster, project_root = getwd()) {
  c(unlist(cluster$setup_lines), sprintf('cd "%s"', project_root))
}

#' A controller's worker limit when config/pipeline.yml sets none: no cap
#' in practice (above SLURM's MaxArraySize, 20001), so SLURM's scheduler
#' and per-user limits decide what runs when.
UNCAPPED_WORKERS <- 50000L

#' Build the pipeline's crew controllers
#'
#' One `crew_controller_slurm()` per `controllers:` entry (plus
#' `<name>_backup` where a `backup:` block is given), combined with
#' `crew_controller_group()`; targets pick one with
#' `tar_resources_crew(controller = "<name>")`. A controller without `workers:`
#' gets UNCAPPED_WORKERS.
#'
#' @param pipeline The parsed pipeline config.
#' @param project_root Project directory (worker logs go to `logs/crew/`).
#' @return A crew controller group.
#' @keywords internal
#' @export
build_controller_group <- function(pipeline, project_root = getwd()) {
  log_dir <- file.path(project_root, "logs", "crew")
  dir.create(file.path(log_dir, "scripts"), recursive = TRUE, showWarnings = FALSE)
  script_lines <- worker_script_lines(pipeline$cluster, project_root)

  slurm_controller <- function(name, spec, backup = NULL) {
    crew.cluster::crew_controller_slurm(
      name = name,
      workers = spec$workers %||% UNCAPPED_WORKERS,
      seconds_idle = spec$seconds_idle %||% 30,
      # Soft wall time: after this the worker finishes its current task and
      # exits, and crew launches a fresh one if tasks remain -- so a worker
      # isn't killed by SLURM's --time mid-fit. Needs
      # time_minutes * 60 >= seconds_wall + the longest single fit.
      seconds_wall = spec$seconds_wall %||% (spec$time_minutes * 60 / 2),
      # tasks_max = 1: the worker runs one task and exits -- for methods
      # whose single fits can take hours (spca), so a long fit never starts
      # late in a worker's life.
      tasks_max = spec$tasks_max %||% Inf,
      crashes_max = spec$crashes_max %||% 5L,
      backup = backup,
      options_cluster = crew.cluster::crew_options_slurm(
        script_lines = script_lines,
        script_directory = file.path(log_dir, "scripts"),
        log_output = file.path(log_dir, "%x_%j.out"),
        log_error = NULL,  # stderr goes to log_output too
        memory_gigabytes_required = spec$memory_gigabytes_required,
        cpus_per_task = spec$cpus_per_task,
        time_minutes = spec$time_minutes,
        partition = pipeline$cluster$partition
      )
    )
  }

  controllers <- list()
  for (name in names(pipeline$controllers)) {
    spec <- pipeline$controllers[[name]]
    # A `backup:` block (crew's backup controllers, see the "Backup
    # controllers" section of crew's groups vignette): a task whose worker
    # crashes `crashes_max` times in a row on this controller -- e.g. SLURM
    # killing it for exceeding --mem -- is retried on "<name>_backup",
    # whose spec is this one with the `backup:` fields overridden. targets
    # (>= 1.10.0.9002) does the retry automatically.
    backup <- NULL
    if (!is.null(spec$backup)) {
      backup_spec <- utils::modifyList(spec[setdiff(names(spec), c("backup", "crashes_max"))], spec$backup)
      backup <- slurm_controller(paste0(name, "_backup"), backup_spec)
    }
    controllers[[name]] <- slurm_controller(name, spec, backup)
    if (!is.null(backup)) controllers[[paste0(name, "_backup")]] <- backup
  }
  do.call(crew::crew_controller_group, unname(controllers))
}
