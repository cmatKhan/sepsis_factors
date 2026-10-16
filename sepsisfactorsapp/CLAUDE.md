# CLAUDE.md

Guidance for working in this package; the repository-wide rules are in the root `CLAUDE.md`.

`sepsisfactorsapp` is the Shiny app. It reads a schema-4 database written by `sepsisfactorsdb`,
read-only, from a path the user supplies (`run_app(db_path)` or `STABILITY_DB`; `download_db()` fetches
the published one). Never copy data into the package and never write to the database. `app()` refuses
any other schema version; `EXPECTED_SCHEMA_VERSION` is the one value that changes with a schema bump, and
a test fails when it differs from `SCHEMA_VERSION` in `sepsisfactorsdb`.

- The app runs from the database file alone and reads no config files. Dataset family and timepoint
  order, composition markers, the documentation link, and method labels, colours, order and blurbs come
  from the database. A `stability_artifacts/` folder beside the database is optional: `artifacts.R` is the
  only code that calls `readRDS()`, and loadings, scores, projections and pattern drivers use the full file
  when it exists and the reduced copy in the database otherwise. A page that loses something by falling
  back shows `reduced_notice()` and says only what that page loses.
- `db_*.R` are the only files that write SQL, always with bound parameters; the open database is a handle
  (`open_db()`) passed to every function. A missing table is the classed condition
  `sepsisfactorsapp_missing_table` (`if_missing()` turns it into a notice). Missing data is noted on the page
  via `notes.R` (`note_missing()`, `need_data()`), never fatal.
- The app keeps a few copies of helpers from the other packages and does not depend on them: the blob
  decoders in `blobs.R` (the writer's `decode_f32()`, `decode_i32()`, JSON ids). They are listed in the root
  `TODO.md`, section 1. Do not add a dependency on the other packages.
- No environments, `<<-` or `assign()` as mutable state. Per-session values are reactives (`bindCache()`
  where a result is costly).
- Method facts come from the `methods` table (`label`, `color`, `display_order`, `blurb`, `family`,
  `has_loadings`). Two cases stay by name because the schema does not carry them: PCA is one fit whose first
  n components stand for rank n, and ICASSO quality exists only for ICA.
- bslib page layouts and components (`page_navbar()`, `sidebar()`, `card()`, `nav_panel()`); use the
  `shiny:shiny-bslib` skill. Column names used in `aes()` are declared in `R/sepsisfactorsapp-package.R`.
- Tests: `devtools::test()` runs the unit tests against a tiny committed schema-4 fixture
  (`tests/testthat/fixtures/stability.sqlite`, rebuilt by `dev/make_fixture.R`; the app itself never imports
  `sepsisfactorsdb`). Open it once per test file. The page tests (`test-app_server.R`) need a real database:
  set `STABILITY_DB` to a schema-4 database (they skip without it). A page output that stops with
  `validate()`/`need()` is the app's "Not available" notice, not a failure. Keep the suite fast.
- Lint: the R files have no lintr lints (`prek run --files <changed>`); keep it so.
