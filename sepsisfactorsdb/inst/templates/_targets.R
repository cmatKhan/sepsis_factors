# Ingest workflow: loads a sepsisfactors results output into the stability
# database. Run it with targets::tar_make(); a dataset whose results did not
# change is skipped on the next run. If you delete the database file, also run
# targets::tar_invalidate(starts_with("db_")) so the loads run again.
library(targets)

results_root <- "results" # the directory written by sepsisfactors
db_path <- "stability/stability.sqlite"
artifact_mode <- "link" # "link", "copy" or "move"

tar_option_set(packages = "sepsisfactorsdb", error = "stop")

sepsisfactorsdb::ingest_targets(results_root, db_path, artifact_mode = artifact_mode)
