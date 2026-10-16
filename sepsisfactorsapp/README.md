# sepsisfactorsapp

A Shiny app for exploring the latent-factor stability results: overview and
variance by dataset, comparing methods and datasets, exploring fits, inspecting
factors, and following factors across time and datasets.

The app only **reads**. It opens the SQLite database written by
`sepsisfactorsdb` (schema version 3) read-only and loads the artifact files
(`stability_artifacts/`) from beside it. Nothing is copied into this package:
you say where the database is.

## Run

```r
sepsisfactorsapp::run_app("/path/to/stability.sqlite")
# or
Sys.setenv(STABILITY_DB = "/path/to/stability.sqlite")
sepsisfactorsapp::run_app()
```

`inst/app/app.R` is the same call, for `shiny::runApp(system.file("app", package = "sepsisfactorsapp"))`
or deployment (set `STABILITY_DB` in the environment).

A database that is not at schema version 3 is refused with an error. A database
in WAL mode on a read-only file system (a mounted snapshot) is opened as an
immutable snapshot; restart the app after replacing the file.

### Optional configuration

`config_dir` (or the environment variable `SEPSISFACTORSAPP_CONFIG`) names a
directory with `dataset_families.yml` (timepoint order within a dataset family),
`composition_markers.yml` (marker genes behind the composition scores) and the
datasets' `*_config.yml` files (links to the documentation). Without it the app
orders timepoints by the number in the dataset id, leaves out the marker list
and links, and logs what is missing to the console. The `sepsisfactors`
package ships these files (`inst/config/`, `inst/datasets/`).

## Tests

`devtools::test()` runs the unit tests. The tests that drive the pages need a
database: set `STABILITY_DB` to a schema-3 database first. They read it where
it is.

## Status

This is a port of the app from the original repository, kept behaviour-for-
behaviour; see `NEWS.md`. Decoupling it from the other packages (a typed data
layer, no copied helpers) comes later.
