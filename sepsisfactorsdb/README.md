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

```r
library(sepsisfactorsdb)

# validate, then load every dataset and the cross-dataset tables
ingest_results("path/to/results", "path/to/stability.sqlite")
```

For a parallel build (one part file per dataset, merged), locally or on SLURM:

```r
ingest_parallel("path/to/results", "path/to/stability.sqlite",
                controller = ingest_controller("local"))
```

For an incremental, restartable load, set up a targets workflow (`ingest.yml` chooses local or SLURM
workers; `use_ingest(serial = TRUE)` sets up the serial one instead):

```r
use_ingest("ingest")   # writes ingest/_targets.R; edit the paths, then targets::tar_make()
```

## Documentation

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
