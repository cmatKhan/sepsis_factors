# CLAUDE.md

Guidance for working in this package; the repository-wide rules are in the root `CLAUDE.md`.

`sepsisfactorsdb` is the loader. It takes the results output that `sepsisfactors` writes, validates it,
and loads it into the SQLite database (schema 4) and the artifact layout that `sepsisfactorsapp` reads.
**No statistics here**: if a number has to be computed, it belongs in `sepsisfactors`'s results output.
**TEMPORARY exception**: the derive step (`R/derive*.R`, run by `ingest_results()` after each load) computes
reduced and precomputed tables (association scan, cross-method agreement, top genes, ...) from the full
artifacts, and `legacy_to_results()` (`R/legacy_adapter.R`) reshapes the original project's schema-3 output.
Both go when `sepsisfactors` writes the contract; section 7 of the root `TODO.md` lists every piece, the
packages to remove (`yaml`, `clue`, `mclust`) and the contract catch-up. Add nothing else that computes.

- It specifies both contracts. `vignette("results-output")` is the results output (one Parquet file per
  database table with natural keys instead of ids, plus a per-method output spec);
  `vignette("database")` is the schema and artifacts. Keep them current when either changes.
- Versions: `RESULTS_OUTPUT_VERSION` (whole contract), `SUPPORTED_METHOD_OUTPUTS` (per-method
  `method_output_version`s the loader can ingest), `SCHEMA_VERSION` (database layout). An unknown method
  or version is a clear error before anything is written. `inst/sql/schema.sql` and `views.sql` define
  schema 4; changing them means bumping `SCHEMA_VERSION` and telling `sepsisfactorsapp`.
- The parallel build (`R/parallel.R`, `parts.R`, `merge.R`, `controllers.R`) writes one part file per dataset (and
  chunk) and merges them; the merge renumbers surrogate ids from the schema, so no writer does id arithmetic.
  `ingest_parallel()` must equal `ingest_results()` table by table, ids included (tests compare them). `crew` and
  `crew.cluster` are permanent dependencies.
- Loading is idempotent: ingesting the same results twice leaves the same rows and the same fit ids.
  Artifacts are placed in the database's own tree; `remove_orphan_artifacts()` never touches the results
  output.
- The ingest workflow (`flow.R`, `use_ingest()`) is a targets project kept for its restart behaviour; if
  it is more trouble than it is worth, remove it.
- After any change to `inst/sql/schema.sql`, run `Rscript data-raw/schema_columns.R`: it saves the column lists
  as internal data (`R/sysdata.rda`), which `contract_columns()` and the loaders read instead of the schema
  (reading it costs ~40 ms and is needed hundreds of times per ingest). `test-schema.R` fails if they differ.
- Do not add DBI-free shortcuts around `open_stability_db()`: it enforces the schema version.
- Tests: `devtools::test()`. The fixture is a tiny synthetic results tree (`example_results.R`).
- Schema 4 decisions are in the root `TODO.md` (sections 5 and 6.2); the database is complete on its own
  (derived tables), the artifacts beside it are optional full copies. Dev scripts for a smoke run and a parity
  check against the app are in `dev/`.
