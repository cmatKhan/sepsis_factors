# CLAUDE.md

Guidance for working in this package; the repository-wide rules are in the root `CLAUDE.md`.

`sepsisfactorsdb` is the loader. It takes the results output that `sepsisfactors` writes, validates it,
and loads it into the SQLite database (schema 3) and the artifact layout that `sepsisfactorsapp` reads.
**No statistics here**: if a number has to be computed, it belongs in `sepsisfactors`'s results output.

- It specifies both contracts. `vignette("results-output")` is the results output (one Parquet file per
  database table with natural keys instead of ids, plus a per-method output spec);
  `vignette("database")` is the schema and artifacts. Keep them current when either changes.
- Versions: `RESULTS_OUTPUT_VERSION` (whole contract), `SUPPORTED_METHOD_OUTPUTS` (per-method
  `method_output_version`s the loader can ingest), `SCHEMA_VERSION` (database layout). An unknown method
  or version is a clear error before anything is written. `inst/sql/schema.sql` and `views.sql` define
  schema 3; changing them means bumping `SCHEMA_VERSION` and telling `sepsisfactorsapp`.
- Loading is idempotent: ingesting the same results twice leaves the same rows and the same fit ids.
  Artifacts are placed in the database's own tree; `remove_orphan_artifacts()` never touches the results
  output.
- The ingest workflow (`flow.R`, `use_ingest()`) is a targets project kept for its restart behaviour; if
  it is more trouble than it is worth, remove it.
- Do not add DBI-free shortcuts around `open_stability_db()`: it enforces the schema version.
- Tests: `devtools::test()`. The fixture is a tiny synthetic results tree (`example_results.R`).
- Schema 4 plans are in `TODO.md` (git-ignored).
