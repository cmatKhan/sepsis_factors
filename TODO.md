# TODO (monorepo)

Cross-package items. Package-specific notes are in each package's own `TODO.md` (git-ignored).

## 1. Remove the repeated code and constants (the app port copied them)

`sepsisfactorsapp` was ported at schema 3 with copies of things that live in the other packages, so the
app could be developed against the current results before the packages were fully decoupled. None of the
copies may become a package dependency (no package imports another); each is removed by moving the work
to where it belongs.

| In `sepsisfactorsapp` | Also in | How it goes away |
|---|---|---|
| `R/similarity.R` (`prepare_loadings()`, `pair_similarities()`, `hungarian_match()`, `hungarian_match_abs()`), a verbatim copy used for live cross-method agreement | `sepsisfactors/R/analysis_similarity.R` | Compute cross-method agreement (gene-set Jaccard, sample ARI, matched factors) in `sepsisfactors`, store it, and have the app read it. Then drop the copy, and the app's `clue` and `mclust` dependencies if nothing else uses them. |
| `EXPECTED_SCHEMA_VERSION` in `R/run_app.R` | `SCHEMA_VERSION` in `sepsisfactorsdb/R/schema.R` | Store the version in the database (`PRAGMA user_version` already holds it) and keep the app's expected value as the one place that must change with a schema bump; add a test that fails when the two differ, run in CI from the monorepo. |
| `resolve_artifact()`, the `stability_artifacts/<dataset>/<file>` layout and `options(stability.db_dir)` | `sepsisfactorsdb/R/artifacts.R` | Disappears with item 2 (no artifacts). |
| `METHOD_ORDER`, `METRICS` and the method labels and colours | the closed method set in `sepsisfactors` (`method_manifest()`) and the `methods` table | Store display label, colour and order on the `methods` table (schema 4) and read them. |
| Config the app reads from `config_dir`: `dataset_families.yml`, `composition_markers.yml`, per-dataset `hf_collection` | `sepsisfactors/inst/config/`, `sepsisfactors/inst/datasets/*/config.yml` | Store dataset family and timepoint order, composition markers and `hf_collection` in the database; the app then reads no files and `config_dir` is removed. |
| Plain-comment copies of internal docs, original file names (`db_helpers.R`, `data_helpers.R`, `mod_*.R`) | n/a | Reorganise when the data-access layer is rewritten (item 2). |

Order: item 2 first (it removes the artifact copy and most of what the schema review needs), then the
agreement and config items together as part of schema 4.

## 2. `sepsisfactorsapp` reads the SQLite database only

Goal: the app needs the database file and nothing else. No `stability_artifacts/` directory, no
`readRDS()`, no `options(stability.db_dir)`, no arrow workaround for old metadata files.

Today the app reads these artifact kinds (from a search of the app code; confirm with the review below):
- per fit: `loadings`, `scores`, `diag`, `dendrogram`;
- per dataset: `sample_metadata`, `feature_metadata`, `variance_partition` (`gene_summary`);
- `enrichment_genes` and `pattern_markers`;
- projection results (`$projection`) and pattern-driver files.

Work:
1. For each kind, decide with the schema review (item 3): store it in the database (as tables, or as a
   compressed blob when it is only ever read whole), or stop needing it (compute less, or precompute the
   summary the app actually shows).
2. Large matrices (loadings and scores for every fit) are the hard part. Options to evaluate with sizes
   from the real database: tables in long form for what pages show (top genes per factor, scores for the
   selected fit), a blob per fit read whole, or both. Measure database size and page load time before
   choosing.
3. Add one data-access layer in the app (`db_*.R`) as the only code touching SQL; a classed
   missing-table condition instead of `safe_query()` returning `NULL`; bound parameters throughout.
4. Remove artifact placement and orphan cleanup from `sepsisfactorsdb` once nothing reads artifacts
   (`artifacts.R`, `place_artifacts()`, `remove_orphan_artifacts()`, the `artifact_mode` argument), and
   the artifact files from the results output contract if they are no longer needed by the loader.
5. Test: the app runs from a copy of the database file alone, with `stability_artifacts/` absent.

## 3. Review the schema against what the app actually uses

Do this before schema 4 is designed. The rule: **store in the database only what the app needs**, but
keep everything computed in the results output (`sepsisfactors`), so a table can be added back to the
database without recomputing.

Method:
1. Inventory what the app uses: every table and column queried in `sepsisfactorsapp/R/` (a first
   grep, not exhaustive, shows 38 tables and views read), and every artifact read (item 2).
2. Inventory the schema: every table, column and view in `sepsisfactorsdb/inst/sql/schema.sql` and
   `views.sql`.
3. Produce one table: for each table and column, which app page uses it, or "unused". A first
   look at table names suggests these are not queried by the app today and need a decision (keep for a
   planned page, move to the results output only, or drop): `methods`, `fit_params`,
   `fit_redundancy`, `similarity_histograms`, `icasso_membership`, `factor_variance`,
   `factor_variance_models`, and several columns of the tables the app does read. Verify each before
   acting; the search may miss tables reached through views.
4. Decide per item: keep, drop from the database (still in the results output), or add. The
   "Open items" of the original project list pages that are planned but not built (time structure on the
   stored variance results, flags for GSE54514 and sex-discordant samples); keep what those pages need.
5. Check the other direction too: queries that join many tables or compute in R what the database could
   hold precomputed (`fit_summary`, same-rank matches, projections with fits, cross-method agreement,
   the optimal-fit flag, association scans) and that schema 4 should store.
6. Write the result as the schema 4 design (changes to `schema.sql`, `views.sql`, the results-output
   spec in `sepsisfactorsdb`'s `results-output` vignette, the writer in `sepsisfactors`, the loader, and
   the app's queries), with `SCHEMA_VERSION` and `RESULTS_OUTPUT_VERSION` bumped together. The existing
   schema 4 notes are in `sepsisfactorsdb/TODO.md`.

## 4. Smaller items

- A small committed fixture database (schema 3, built from `sepsisfactorsdb::example_results()`) so the
  app's page tests run in CI without a real database.
- Run the one-time styler restyle (135 files change) as its own commit, then the package tests; then
  make the lintr hook blocking once the baseline is clean.
- Verify the GitHub workflows on a throwaway issue (plan, auto-fix, respond, release, docs); see the
  README section on the automation.
- Cluster checks (phase 2) and the from-scratch rerun of the pipeline: `sepsisfactors/TODO.md`.
