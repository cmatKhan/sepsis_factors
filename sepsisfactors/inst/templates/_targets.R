# targets workflow: fits every configured method on every dataset, computes
# the stability, enrichment, variance and projection analyses, and writes the
# results output (see pipeline.yml's `results_root`) that sepsisfactorsdb loads
# into the database.
#
# Run from this directory:
#   targets::tar_visnetwork()                            # the graph, and what is outdated
#   targets::tar_make()                                  # build everything outdated
#   targets::tar_make(names = starts_with("fit_pca_"))   # just some targets
# For runs longer than your session: sbatch run_pipeline.sbatch
#
# Each fit_<method>_<dataset> target runs on SLURM workers launched by
# crew.cluster (one controller per method, pipeline.yml). The main process
# (this one) must stay alive for the run: workers connect back to it.

library(targets)

pipeline <- sepsisfactors::read_pipeline_config("pipeline.yml")

tar_option_set(
  packages = "sepsisfactors",
  controller = sepsisfactors::build_controller_group(pipeline),
  # Shared filesystem: workers read their inputs from and write their
  # results to _targets/ directly instead of sending them through the main
  # process.
  storage = "worker",
  retrieval = "worker",
  # One failed fit shouldn't stop the other branches.
  error = "continue"
)

sepsisfactors::pipeline_targets(pipeline)
