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
