# the SLURM controller is built from the config without submitting anything

    Code
      str(lapply(list(default = opts("default"), big = opts("big")), function(o) {
        o$script_lines[2] <- "cd <project>"
        o
      }))
    Output
      List of 2
       $ default:List of 6
        ..$ script_lines             : chr [1:2] "module load R/4.4" "cd <project>"
        ..$ log_output               : chr "logs/crew/%x_%j.out"
        ..$ memory_gigabytes_required: num 8
        ..$ cpus_per_task            : int 1
        ..$ time_minutes             : num 240
        ..$ partition                : chr "short"
       $ big    :List of 6
        ..$ script_lines             : chr [1:2] "module load R/4.4" "cd <project>"
        ..$ log_output               : chr "logs/crew/%x_%j.out"
        ..$ memory_gigabytes_required: num 32
        ..$ cpus_per_task            : int 2
        ..$ time_minutes             : num 480
        ..$ partition                : chr "short"

# ingest.yml supplies defaults and use_ingest writes the files

    Code
      cat(readLines(file.path(dir, "_targets.R")), sep = "\n")
    Output
      # Parallel ingest workflow: loads a sepsisfactors results output into the stability
      # database. Every dataset is loaded into its own part file by a worker (local
      # processes or SLURM jobs, see `controller:` in ingest.yml), the parts are merged
      # and the result replaces the database. Run it with targets::tar_make(), or for a
      # long run sbatch run_ingest.sbatch. A dataset whose results did not change is not
      # rebuilt; a deleted database is rebuilt by the merge alone.
      library(targets)
      
      config <- sepsisfactorsdb::read_ingest_config("ingest.yml")
      
      tar_option_set(
        packages = "sepsisfactorsdb",
        error = "stop",
        controller = sepsisfactorsdb::ingest_controller(config$controller$type, config = config),
        # Shared filesystem: workers write their parts to part_dir themselves.
        storage = "worker",
        retrieval = "worker"
      )
      
      sepsisfactorsdb::ingest_parallel_targets(config)

