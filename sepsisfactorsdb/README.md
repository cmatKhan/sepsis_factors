# sepsisfactorsdb

Loads already-computed sepsis latent-factor results into a SQLite database (schema 4), which is
complete on its own, plus an optional directory of the full artifacts. The results come from the
`sepsisfactors` package as a *results output* (Parquet tables and `.rds` artifacts), and the
database is read by other tools, such as the Shiny app. Temporarily the loader also builds reduced
and precomputed tables from the full artifacts (the derive step) and can convert the original
project's output (`legacy_to_results()`); both go when `sepsisfactors` writes the contract itself
(root `TODO.md`, section 7). The three packages do not import each other; they are joined by two versioned contracts.

```
sepsisfactors --[results output]--> sepsisfactorsdb --[SQLite DB + artifacts]--> sepsisfactorsapp
```

## Use

`vignette("building")` has the full how-to, including the SLURM setup. In short:

**Sequential** (a few datasets, or a quick check):

```r
library(sepsisfactorsdb)

# validate, then load every dataset and the cross-dataset tables
ingest_results("path/to/results", "path/to/stability.sqlite")
```

**Parallel on one machine** (one part file per dataset, built by worker processes, then merged):

```r
ingest_parallel("path/to/results", "path/to/stability.sqlite",
                controller = ingest_controller("local"))
```

**Parallel on a SLURM cluster** (the workers are SLURM jobs, through `crew.cluster`):

```r
sepsisfactorsdb::use_ingest("run")   # writes run/_targets.R, run/ingest.yml, run/run_ingest.sbatch
```

```sh
# 1. install sepsisfactorsdb where workers can see it (R CMD INSTALL sepsisfactorsdb)
# 2. edit run/ingest.yml: results_root, db_path, part_dir (shared), controller.type: slurm,
#    controller.slurm.setup_lines (how R gets on the path), partition, memory and time per profile
# 3. try three small datasets first (datasets: [...] in ingest.yml), then all
cd run && mkdir -p logs && sbatch run_ingest.sbatch
```

All three give the same database, ids included. A failed build never replaces an existing database.
A rerun rebuilds only the parts of datasets whose results changed. The SLURM path has not yet been
run on a real cluster; its resource defaults are starting points.

For an incremental, restartable sequential load, `use_ingest("run", serial = TRUE)` sets up the serial
targets workflow.

The original project's output (a schema-3 database with `stability_artifacts/`) is converted first with
`legacy_to_results()` (temporary; see `vignette("building")`).

## Documentation

- `vignette("building")`: how to build the database, sequentially, in parallel and on SLURM.
- `vignette("results-output")`: the results-output specification (what `sepsisfactors` writes).
- `vignette("database")`: the schema and artifact reference (what readers can rely on).
- `example_results()` writes a tiny valid results output.

## Versions

- `SCHEMA_VERSION` decides whether a database can be opened; the package version never does.
- `RESULTS_OUTPUT_VERSION` and a `method_output_version` per method decide whether a results output
  can be loaded. The method set is closed (`supported_methods()`); a new method, or a changed method
  output, is a developer task.

## Installation

Needs R >= 4.4. Install from a local checkout with `devtools::install()`.
