# CLAUDE.md

Guidance for working in this package; the repository-wide rules are in the root `CLAUDE.md`.

`sepsisfactorsapp` is the Shiny app. It reads a schema-3 database written by `sepsisfactorsdb`,
read-only, from a path the user supplies (`run_app(db_path)` or `STABILITY_DB`). Never copy data into
the package and never write to the database. `app()` refuses any other schema version.

- It is a port of the original app, kept behaviour-for-behaviour. The `R/` files keep their original
  names (`db_helpers.R`, `data_helpers.R`, `mod_*.R`, ...), internal documentation is plain comments, and
  `R/similarity.R` is a copy of `sepsisfactors`' matching code. Do not add a dependency on the other
  packages; schema 4 removes the copies (see the root `TODO.md`, section 6.3).
- Optional configuration comes from `config_dir` / `SEPSISFACTORSAPP_CONFIG`; without it the app degrades
  with a console note, never an error. Missing data is noted on the page via `notes.R`, never fatal.
- bslib page layouts and components (`page_navbar()`, `sidebar()`, `card()`, `nav_panel()`); use the
  `shiny:shiny-bslib` skill. Column names used in `aes()` are declared in `R/sepsisfactorsapp-package.R`.
- Tests: `devtools::test()` runs the unit tests. The page tests need a database: set `STABILITY_DB` to a
  schema-3 database (they skip without it). A page output that stops with `validate()`/`need()` is the
  app's "Not available" notice, not a failure.
