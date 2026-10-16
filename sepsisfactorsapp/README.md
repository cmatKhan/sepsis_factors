# sepsisfactorsapp

A Shiny app for exploring the latent-factor stability results: overview and
variance by dataset, comparing methods and datasets, exploring fits, inspecting
factors, and following factors across time and datasets.

The app only **reads**. It opens the SQLite database written by
`sepsisfactorsdb` (schema version 4) read-only. The database file alone is
enough; nothing is copied into this package, and the app reads no configuration
files: you say where the database is.

## Install

The app is a standalone R package. It does not need `sepsisfactors` or `sepsisfactorsdb`
installed: it only reads the database file they produce.

From GitHub, with [pak](https://pak.r-lib.org/):

```r
install.packages("pak")   # once, if you do not have it
pak::pak("cmatKhan/sepsis_factors/sepsisfactorsapp")
```

From a local checkout of the repository:

```r
install.packages("devtools")   # once
devtools::install("sepsisfactorsapp")
# or from a shell, in the directory that contains sepsisfactorsapp/:
#   R CMD INSTALL sepsisfactorsapp
```

The packages it uses (bslib, cli, DBI, DT, ggplot2, ggrepel, jsonlite, RSQLite, scales, shiny,
withr) come from CRAN and are installed with it. Check the install by loading it:

```r
library(sepsisfactorsapp)
```

`download_db()` below needs an internet connection and nothing beyond base R.

### Update during active development

The package changes often and its version number (`0.0.0.9000`) does not change between commits, so
the version cannot tell you whether you have the latest code. To be sure you get the latest commit,
remove the installed copy and install it again. Start from a fresh R session (restart R first: a
package that is loaded in the session cannot be replaced):

```r
pak::pkg_remove("sepsisfactorsapp")
pak::pak("cmatKhan/sepsis_factors/sepsisfactorsapp")
```

`pkg_remove()` removes the package from the first library on `.libPaths()` (give `lib =` for
another one) and leaves its dependencies alone. The install then takes the latest commit of the
repository's default branch. The repository holds three packages, so any commit to it can lead to a
new install, even if the app itself did not change.

## Get the data and run

```r
sepsisfactorsapp::download_db()                      # saves db_no_rose.sqlite here
sepsisfactorsapp::run_app("db_no_rose.sqlite")
```

`download_db()` fetches the published database (several gigabytes) into the
current directory, or `dest_dir`. It downloads under a temporary name and
renames the file when it is complete, then checks that it is an SQLite database
at the schema version this app reads. An existing file is kept unless
`overwrite = TRUE`. The published file holds the datasets that are not ROSE
datasets; ROSE datasets are not in it.

To use your own database:

```r
sepsisfactorsapp::run_app("/path/to/stability.sqlite")
# or
Sys.setenv(STABILITY_DB = "/path/to/stability.sqlite")
sepsisfactorsapp::run_app()
```

`inst/app/app.R` is the same call, for `shiny::runApp(system.file("app", package = "sepsisfactorsapp"))`
or deployment (set `STABILITY_DB` in the environment).

A database that is not at schema version 4 is refused with an error. A database
in WAL mode on a read-only file system (a mounted snapshot) is opened as an
immutable snapshot; restart the app after replacing the file.

## Reduced data and the optional `stability_artifacts/` folder

The database holds reduced copies of the largest results: the top 50 genes per
side of each factor instead of the full loadings, the significant genes only of
each pattern-driver result, and the samples-by-factors scores and projections
as compressed single-precision blobs. Cross-method agreement and the
factor-by-metadata associations are precomputed.

If the `stability_artifacts/` folder written with the database sits beside it,
the pages use the full loadings, scores, projections and pattern-driver results
from it instead. Without it, a page that loses something says so in a
"Reduced data" notice:

| Page | What is reduced without the folder |
|---|---|
| Biplots, Genes tab | Gene arrows are chosen from the stored top genes of each factor; other loadings count as zero |
| Pattern drivers (Welch tests) | Only significant or shared genes; the volcano has no non-significant background |
| Projection comparison, Gene-space alignment | Not available: it needs the full loadings |

The other pages show the same content either way. The complete data come from
the `sepsisfactors` output with the `stability_artifacts/` folder beside the
database.

## Tests

`devtools::test()` runs the unit tests. The data-access tests read a tiny
committed schema-4 database (`tests/testthat/fixtures/stability.sqlite`),
rebuilt by `dev/make_fixture.R` from `sepsisfactorsdb`'s example results. The
tests that drive the pages against a real database are opt-in: set
`STABILITY_DB` to a schema-4 database first.

## Status

The app was ported from the original repository and is now rebuilt on schema 4;
see `NEWS.md`.
