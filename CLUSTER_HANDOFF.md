# Handoff: finish and verify the parallel / SLURM database build on the cluster

For a Claude Code session on the SLURM cluster. Written 2026-10-16 at the end of a local session. Everything
named here is in `sepsisfactorsdb/` unless stated. Read the root `CLAUDE.md` and `sepsisfactorsdb/CLAUDE.md`
first (ask before hacking; no commits to `main`; messages through cli; usethis for dependencies).

## What this is for

Build the schema-4 stability database faster. `sepsisfactorsdb` now builds one SQLite **part file** per
dataset (and per chunk of fits for large datasets) in parallel on crew workers, then merges the parts in
SQLite (`ATTACH` + `INSERT ... SELECT`), renumbering ids, and replaces the final database atomically. One
option picks where the workers run: local processes (`crew`) or SLURM jobs (`crew.cluster`). It was
implemented and tested locally. **No SLURM job has ever been submitted from this code.** Your job is to run it
on the cluster, fix what only a real cluster shows, and report honestly what worked.

## State at handoff (agent's report, quoted; not re-run by the coordinator)

- `devtools::test()`: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 374 ]`. `devtools::check()`: 0 errors, 0 warnings,
  0 notes. `prek`: all hooks passed (lintr is non-blocking and reports style on test files).
- Equivalence: on synthetic data and on three real legacy datasets (`GSE54514_DAY3/4/5`, 4 local workers,
  `chunk_fits = 100`), the parallel database equals the serial one in all 47 tables, ids included (about
  1.98 M rows); `PRAGMA foreign_key_check` is empty. Wall time: serial 141.6 s, parallel 85.2 s (small data,
  includes worker start-up and merge). Extrapolation to all 35 datasets (not measured): about 40-60 M rows,
  merge 1-2 min plus indexes and a copy of about 2.5 GB; about 30 min on ~35 SLURM workers, 30-40 min locally
  on 10 workers.
- **Not verified anywhere:** job submission, worker start-up under `setup_lines`, the generated worker script,
  node-local `work_dir` with shared `part_dir`, memory sizing, `run_ingest.sbatch`, hundreds of workers,
  and the full 35-dataset build.
- Uncommitted work: the whole tree was uncommitted when this was written. Check `git log` for whether it was
  committed and pushed before you started.

## Design in one page

- **Units of work.** `build_load_part()` per dataset (load + dataset-level derive). A dataset with more than
  `chunk_fits` units of work (fits plus pattern-driver files; default 400) also gets `build_chunk_part()` parts
  for its fit-level derived tables. `build_projection_part()` handles cross-dataset projection blobs per source
  dataset. See `R/parts.R`.
- **Merge** (`R/merge.R`, `merge_parts()`): builds the database in `merge_dir` (local disk) with journaling off,
  inserts table by table in foreign-key order, creates indexes after, checks foreign keys and integrity
  (`check_database()`), then copies to `db_path` under a temporary name and renames. A failed build leaves the
  old database untouched (tested by md5).
- **Ids are renumbered at the merge.** Parts use plain SQLite autoincrement; the merge shifts each table's id
  column and every foreign key to it by the highest id already in the final file, found from the schema
  (`PRAGMA foreign_key_list`), merging datasets sorted by id with chunk parts right after their load part. The
  serial path sorts datasets the same way, so ids are identical. Ids can shift when an earlier dataset changes.
  Documented in the `database` vignette ("Building the database in parallel") and in `?merge_parts`.
- **Controllers** (`R/controllers.R`, `ingest_controller(type = c("local", "slurm"))`): a `crew` controller
  group with two controllers, `default` (a dataset load or small chunk) and `big` (a chunk of a large dataset),
  chosen per target with `tar_resources_crew()`. SLURM resources come from `ingest.yml`
  (`controller.slurm`: partition, cpus, memory, time, `setup_lines`, `log_dir`, `workers`, `big`). The idioms
  follow `sepsisfactors/R/targets_controllers.R` and `sepsisfactors/inst/templates/` (read them; do not import
  `sepsisfactors`).
- **Orchestration** (`R/parallel.R`): `ingest_parallel_targets(config)` is a targets pipeline; `ingest_parallel()`
  runs it in the current session (`callr_function = NULL`, because the controller is an R object).
  `use_ingest("dir")` writes `_targets.R`, `ingest.yml` and `run_ingest.sbatch` (templates in `inst/templates/`).
  The `merged` and `database` targets run in the main process (`deployment = "main"`), so the main job needs
  memory for the merge (the template asks 64 GB). `database` is a file target: deleting the database rebuilds
  only the merge and publish, with no `tar_invalidate()`.
- `part_dir` must be a **shared** filesystem (the main process merges parts built on other nodes); `work_dir`
  is where a worker builds a part (node-local, e.g. `$TMPDIR`) and parts are moved to `part_dir` when complete.
  The agent changed the spec's "node-local default" for `part_dir` for exactly this reason.

## Inputs you need on the cluster

1. The sepsis_factors repository checkout (with this work), and the package installed into a library visible
   to **workers**: workers load `sepsisfactorsdb` by name (`tar_option_set(packages = "sepsisfactorsdb")`), so
   `devtools::load_all()` is not enough. Install with `R CMD INSTALL sepsisfactorsdb` or `pak::pak("local::sepsisfactorsdb")`.
   Dependencies include `crew`, `crew.cluster`, `targets`, `arrow`, `yaml`, `clue`, `mclust`, `DBI`, `RSQLite`.
2. Either the **legacy tree** from the original project (`stability.sqlite` + `stability_artifacts/`, 28 GB, was a
   read-only squashfs locally) converted with `legacy_to_results()`, or a real `sepsisfactors` results output
   once that package writes results-output version 2 (it still writes version 1; see the root `TODO.md`
   section 7). Neither is in git.
3. `sepsisfactors/inst` for dataset families, composition markers and `hf_collection` (the adapter's `config_dir`).

## Steps

1. **Install and sanity check** locally on a login node: `devtools::test()` on a subset
   (`devtools::test(filter = "merge|parallel")`), then `ingest_controller("slurm")` builds without submitting.
2. **Convert** the legacy tree (the adapter is serial; fine for a one-off, parallelising it is optional):
   `sepsisfactorsdb::legacy_to_results(legacy_root, "run/results", config_dir = "sepsisfactors/inst", artifact_mode = "auto")`.
   `"auto"` hard-links on one filesystem and symlinks across filesystems; it never copies silently.
3. **Configure**: `sepsisfactorsdb::use_ingest("run")`, then edit `run/ingest.yml`:
   `results_root`, `db_path`, `datasets` (start with 3 small datasets, e.g. `GSE54514_DAY3/4/5`),
   `artifact_mode`, `part_dir` (shared), `work_dir` (node-local, e.g. `/tmp` or `$TMPDIR`),
   `merge_dir` (local disk of the main node), `controller.type: slurm`,
   `controller.slurm.setup_lines` (how R gets on the path on a worker, the same lines as in `run_ingest.sbatch`),
   `partition`, memory and time per profile.
4. **Smoke on SLURM**: `cd run && sbatch run_ingest.sbatch`; watch `logs/ingest_main_<jobid>.out`,
   `logs/crew/`, `squeue`. Confirm workers start, parts appear in `part_dir`, the database appears.
5. **Verify equality**: build the same 3 datasets serially (`ingest_results()`) and compare table by table
   including ids (use `differing_tables(a, b)` from `sepsisfactorsdb/tests/testthat/helper-results.R`, as `test-parallel.R` does; `dev/smoke_parallel.R` only times the two builds).
6. **Full run**: remove the `datasets:` restriction and run again. Record per-target times
   (`targets::tar_meta(fields = c("seconds"))`), worker memory (`sacct -j <id> --format=JobID,MaxRSS,Elapsed`),
   and the final database size. Tune `controller.slurm` resources, `chunk_fits`, `workers` from these.
7. **Report** to the user: what ran, times, memory, failures, and every change you made.

## Things to look at first (suspicions, not findings)

- The how-to for users is `sepsisfactorsdb/vignettes/building.Rmd` (written after this handoff); correct it with what you learn on the cluster. `sbatch` needs `logs/` to exist before submission (the template now says so).
- `script_lines` in `ingest_controller()` includes `cd "<getwd() at build time>"`: check it is right for the
  working directory the main job and workers actually use.
- `storage = "worker"`, `retrieval = "worker"` need the `_targets` store and `part_dir` on a shared filesystem.
- `workers: 200` (default profile) and 20 (`big`) may exceed per-user job limits; check the cluster policy.
- Peak memory of the main job at the merge is unmeasured at full size (database about 2.5-3 GB; the template
  asks 64 GB). Memory of a worker for the largest chunk (MARS, GSE95233, ANEMONES) is a guess (8 GB / 32 GB).
- Gene-id mismatches: the derive step errors on any loadings, driver or dendrogram gene id missing from a
  dataset's feature metadata. Only a few datasets were checked; expect to hit one in the full run, and fix the
  adapter or report it; do not loosen the check without asking.
- `artifact_mode: link` needs results and the database directory on one filesystem; otherwise use `symlink`.
- Behaviour with many simultaneous workers writing parts to a network `part_dir` is untested.

## Out of scope (do not do)

- The Shiny app (`sepsisfactorsapp/`) still reads columns and tables schema 4 dropped; it is the next stage and is
  waiting. Do not edit it.
- Do not edit `sepsisfactors/` (a later refactor moves the derive step and the adapter there; the list of
  temporary pieces and the packages to remove then is the root `TODO.md`, section 7).
- Do not write into the legacy tree. Do not commit to `main`; in an interactive session do not commit unless
  asked.

## Where to find more

- Root `TODO.md`: section 5 (decisions, benchmark, parallel-build feasibility), section 6 (per-package items),
  section 7 (temporary patches and the packages to remove at the `sepsisfactors` refactor).
- `sepsisfactorsdb/vignettes/database.Rmd` (schema 4, ids, parallel build), `results-output.Rmd` (contract v2).
- `sepsisfactorsdb/dev/smoke_parallel.R`: a runnable local example that times a serial and a parallel build on a few datasets.
- Benchmarks behind the payload sizes: `results_new/bench/summary.csv` (local only), table in `TODO.md` section 5.
