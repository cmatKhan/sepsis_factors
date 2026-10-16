#!/bin/bash
#SBATCH --job-name=ingest_main
#SBATCH --output=logs/ingest_main_%j.out
#SBATCH --cpus-per-task=2
#SBATCH --mem=64G
#SBATCH --time=2-00:00:00

# Run the parallel ingest workflow's main process (`tar_make()`) as a SLURM job.
# The main process submits the worker jobs through crew.cluster, merges the part
# files and publishes the database, so it needs the memory for the merge (about
# the size of the database) and a wall time that covers the whole run.
#
# usage, from the project root (where _targets.R and ingest.yml are):
#   sbatch run_ingest.sbatch
#
# Follow progress with `tail -f logs/ingest_main_<jobid>.out`, or from R with
# targets::tar_progress_summary().

set -euo pipefail

# ------ Get R on PATH ------
# The lines that provide R on your cluster (the same as `setup_lines` in
# ingest.yml, so the workers match), e.g.:
#   module load R/4.6.1
# or a spack activation:
#   eval "$(spack env activate --sh /path/to/spack_env)"
#   unset R_LIBS R_LIBS_USER R_LIBS_SITE

cd "${SLURM_SUBMIT_DIR}"
mkdir -p logs

Rscript -e 'targets::tar_make()'
