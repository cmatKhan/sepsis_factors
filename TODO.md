# TODO (monorepo)

All open items for the three packages, in one place (the per-package TODO files were consolidated here on
2026-10-16). Package-specific items are in section 6.

## 1. Remove the repeated code and constants (the app port copied them)

`sepsisfactorsapp` was ported at schema 3 with copies of things that live in the other packages. None of the
copies may become a package dependency (no package imports another). Status after the app moved to schema 4
(2026-10-16):

| Was in `sepsisfactorsapp` | Status |
|---|---|
| `R/similarity.R` (`prepare_loadings()`, `pair_similarities()`, `hungarian_match*()`), live cross-method agreement | **Removed.** The app reads the precomputed `method_agreement`; `clue` and `mclust` are gone from its `Imports`. The gene-space alignment, which needed a one-to-one match, now orders axes with a greedy match on \|coefficient\| (`greedy_match_abs()`); see the open questions in 6.3. |
| `EXPECTED_SCHEMA_VERSION` in `R/run_app.R` | **Kept as the one value to change with a schema bump**; `tests/testthat/test-run_app.R` fails when it differs from `SCHEMA_VERSION` in `sepsisfactorsdb/R/schema.R` (skipped when the sibling directory is absent). |
| `resolve_artifact()`, `options(stability.db_dir)` | **Removed.** `R/artifacts.R` resolves the database's pointer paths relative to the database directory and is the only caller of `readRDS()`; the database is a handle passed to the functions. |
| `METHOD_ORDER`, `METHOD_LABELS`, `METHOD_COLORS`, `EXPLORE_BLURB`, `METRICS` | **Removed**, except the metric lists: the app reads label, colour, order and blurb from the `methods` table. `STABILITY_METRICS` (cosine, pearson, spearman) is what has a stored per-factor stability; `MATCH_METRICS` adds `jaccard` for the matched-similarity queries. |
| `config_dir`, `dataset_families.yml`, `composition_markers.yml`, `*_config.yml`, the `yaml` dependency | **Removed.** Family and timepoint order, composition markers and the documentation collection come from `datasets.family`, `family_position`, `hf_collection` and `composition_markers`. |
| `arrow` for old metadata artifacts | **Removed.** |

**Copied helpers that remain (listed so they can be kept in step):**

| In `sepsisfactorsapp` | Also in | Note |
|---|---|---|
| `R/blobs.R`: `decode_f32()`, `decode_i32()`, `decode_ids()` | `sepsisfactorsdb/R/derive.R` (`decode_f32()`, `decode_i32()`; ids are `json_ids()`) | The blob format (float32/int32, column-major, little-endian, gzip) is specified in the `database` vignette of `sepsisfactorsdb`. When the derive step moves into `sepsisfactors`, the database vignette stays the contract; change both decoders if the encoding changes. |
| `STABILITY_METRICS`, `MATCH_METRICS`, the `top 50 genes per side` figure in the notices | `sepsisfactorsdb` `TOP_GENES_PER_SIDE`, `match_scores` metric `CHECK` | The notices state the reduction; update them if the derive constants change. |

## 2. `sepsisfactorsapp` reads the SQLite database only

**Status (2026-10-16): done in the app** (see 1 and 6.3); the real-database check is left to the user.

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
4. ~~Remove artifact placement and orphan cleanup from `sepsisfactorsdb`.~~ Reversed (2026-10-16): the app
   keeps an optional full-artifact path for pattern drivers, loadings, scores/diag and projections, so
   `artifacts.R`, `place_artifacts()` and `artifact_mode` stay; the database holds a reduced copy so the
   app works without them.
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
   schema 4 details are in section 6.2.

## 4. Smaller items

- A small committed fixture database (schema 3, built from `sepsisfactorsdb::example_results()`) so the
  app's page tests run in CI without a real database.
- Run the one-time styler restyle (135 files change) as its own commit, then the package tests; then
  make the lintr hook blocking once the baseline is clean.
- Verify the GitHub workflows on a throwaway issue (plan, auto-fix, respond, release, docs); see the
  README section on the automation.
- Cluster checks (phase 2) and the from-scratch rerun of the pipeline: section 6.1.

## 5. Schema 4 work: inventory and decisions (2026-10-16)

Decisions: `results/` (read-only) is a loaded schema-3 tree from the original project, not a results
output. A throwaway converter in `tools/` writes a contract-layout results output to `results_new/`
(git-ignored); `sepsisfactorsdb` loads it into `results_new/stability.sqlite`. The app runs from the
database alone; for pattern drivers, loadings, scores/diag and projections the database holds a reduced
copy and the full artifact is used when present (pages say when data is reduced). Plan:
`~/.claude/plans/playful-wobbling-flute.md`.

### Not used by the app today (drop from the database, or give a use)
- Tables: `fit_params` (only the `para`/`power` pivot in `v_fits`), `similarity_histograms`,
  `icasso_membership`, `fit_redundancy`, `factor_variance_models`, `factor_variance`.
- Artifact kinds: `matrix`, `de_results`, `composition`, `enrichment_genes`, `pattern_markers`, `raw`,
  `redundancy`, `kme`.
- Columns: `datasets` (`description`, `n_samples`, `n_genes`, `ensembl_col`, `*_source`, `written_at`);
  `matrix_diagnostics` (all but `val_sd`); `fits` (`fit_key`, `error`, `written_at`); `fit_pairs`
  (`n_factors_a/b`); `fit_pair_null` (all but `p95`); `icasso_clusters` (`centrotype_factor_id`,
  `dendro_file`); `module_kme_summary` (`sd`, `p99`); `wgcna_sft` (all but `sft_r_sq`); `enrichment_*`
  (`query_size`, `queried_at`, `es`, `log2err`, `term_id`, `is_main_pathway`); `projections` (all but
  `source_fit_id`, `target_dataset_id`, `projection_type`, `relative_r2`, `path`); `pattern_drivers`
  (`computed_at`, `n_*`); `variance_summary` (`*_vw_*`, `rho_*`, timings, `n_control_probes`, ...);
  `variance_terms` (`n_genes`, `mean`, `q10`); `samples` (expression and library-size columns).
- Latent risk (not a current defect): the app never filters `projections` on `include_intercept`. In `results/`
  it is perfectly confounded with `projection_type` (within_dataset = 0, cross_dataset = 1, no pair has both),
  so nothing mixes today; make the app filter, or the schema forbid both variants. Open analysis question
  (with the NMF/CoGAPS shift question in 6.1): within- vs cross-dataset `relative_r2` (0.67 vs 0.39) also
  differs in intercept use. Defect fixed in the app (2026-10-16): the matched-similarity queries accept `jaccard`
  (`MATCH_METRICS`), and projections are filtered on `include_intercept` explicitly.

### Read from artifacts or config today (to move into the database)
`loadings`, `scores`, `diag` (`mse_by_rank`, `sdev`), `dendrogram`, `sample_metadata`, `feature_metadata`,
`variance_partition$gene_summary`, projection files (`projection`; files also hold `pval`, `r_squared`),
pattern-driver files; `dataset_families.yml`, `composition_markers.yml`, `hf_collection`; constants
`METHOD_ORDER`, `METHOD_LABELS`, `METHOD_COLORS`, `EXPLORE_BLURB`.

### Known deviations: `results/` vs what `sepsisfactors` writes (for downstream work)
- `results/` has no manifest or parquet; it is the loaded form.
- Variance QC lives in `results/variance/<dataset>` (PNG/CSV); the writer sends it to `qc/variance`.
- Section 6.1: the one-to-one Ensembl map and the revised fgsea/projectR may change content.
- SHIP-TREND has no sample metadata; variance artifacts exist for 12 parent datasets only.
- Whatever the converter computes that the writer does not yet emit (reduced loadings, cross-method
  agreement, association scan, driver reductions) must move into `sepsisfactors`.

## 6. Per-package items (consolidated 2026-10-16 from the package TODO files)

### 6.1 `sepsisfactors`

Open scientific question (preserved, not changed in the port):
- Projections use the unshifted matrix `mat` for every target, though NMF and CoGAPS are fit on `mat_nn`
  (`mat - rowMins`). Check whether that asymmetry is intended. Until resolved, the `database` vignette
  notes the caveat on projection values.

Deferred:
- `gains`: its preprocessing script is a stub and pooling its four accessions (E-MTAB-4421, 4451, 5273,
  5274) needs a decision; it is not built into the package. Adding it reruns every projection.
- Controller tuning from a full run on the cluster: sPCA on the new grid, every method on SHIP-TREND and
  MARS, `vp` from the cgroup `$resources` records; fgsea is already 6 GB / 3 CPUs (observed peak 3.9 GB).
- The full from-scratch rerun, on the cluster, after phase 2 verification.
- Preprocessing scripts still `library()` their own packages (tidyverse, DESeq2, ...): they are user
  scripts, not package code.
- The `explore/variance_by_time.R` scratch script of the source repo was not ported.
- After schema 4: write the new results-output tables (see 5 and 6.2) instead of the converter computing
  them.

Check on the cluster (phase 2):
- Each method's real output passes `sepsisfactorsdb::validate_results()` (the required artifact kinds in
  `SUPPORTED_METHOD_OUTPUTS` were derived from reading the source code).
- `download_collection()` against the live HuggingFace collection (the default collection slug may list
  only some of the datasets).
- Compare a results tree against the 2026-10-15 run (one-to-one Ensembl map, revised fgsea/projectR).

### 6.2 `sepsisfactorsdb` (schema 4, one bump; decisions in 5)

- **Per-dataset part files + merge: done (stage 2).** `ingest_parallel()` (R/parallel.R, parts.R, merge.R,
  controllers.R): part files per dataset load and per chunk of a large dataset, renumbered at the merge, crew
  local or SLURM controllers from `ingest.yml`, equal to the serial build (tests and a real-data check). Left:
  run it on the cluster (a real SLURM run, node-local `work_dir`, memory of the `default`/`big` profiles and of
  the main job) and, if the full run shows the merge or the main process is the bottleneck, tune
  `chunk_fits` and the profiles.
- **`meta` table** (writer package and version, created_at) for provenance. `SCHEMA_VERSION` stays the
  only gate.
- **Open vocabularies.** Remove the fixed `CHECK` lists on artifact kinds and on `methods.family`;
  generate artifact-kind lists from an artifact registry. Make the `v_fits` parameter pivot generic (it
  hard-codes `para`, `lambda`, `power`, `alpha`). Add display label/colour/order columns to `methods`.
- **Views** replacing multi-table joins in readers: `fit_summary` (stability, in-sample R2, rank per fit),
  same-rank matches, projections joined to fits.
- **Reader inputs in the database** so the app reads no config files: dataset time ordering/family,
  composition markers, `hf_collection`.
- **Precompute** cross-method agreement (gene-set Jaccard, sample ARI) and association scans (computed in
  `sepsisfactors`); the optimal-fit flag is not stored (it depends on the user's threshold and rule).
- **Consequences:** bump `SCHEMA_VERSION` to 4 and `RESULTS_OUTPUT_VERSION`; rebuild the `.sqlite` (and the
  artifact tree if layouts change); load again; update the app's reads and fixtures and `sepsisfactors`'
  writer.
- **Targets framework** (`R/flow.R`, `inst/templates/`): kept for now for its restart behaviour. Remove it
  and the `targets` import if it proves more trouble than it is worth. Known gap: deleting the database
  file does not make targets rerun the loads; run `targets::tar_invalidate(starts_with("db_"))`.
- `air format` was not available when this was written; run `air format .`.

### 6.3 `sepsisfactorsapp`

**Done (2026-10-16, schema 4 stage, no real data used):**
- Reads schema 4 (`EXPECTED_SCHEMA_VERSION` 4, guarded by a test against `sepsisfactorsdb`); runs from the
  database file alone; no config files, no `config_dir`, no `yaml`.
- Dropped `R/similarity.R`, `clue`, `mclust`, `yaml`, `arrow`; the live agreement, association scan and
  curve computations are replaced by reads of `method_agreement`, `factor_association` and `fit_summary`.
- One data-access layer (`db_*.R`) with bound parameters and a database handle; classed missing-table
  condition; `artifacts.R` is the only `readRDS()` caller; reduced-data notices on the pages that lose
  something (biplot gene arrows, Welch-test pattern drivers); gene-space alignment needs the full loadings.
- `download_db()`.
- Unit tests on a committed tiny fixture (`dev/make_fixture.R`); R code lint-clean.

**Left to do with the real database (the user tests this):**
- Open every page against the cluster-built database, with and without the `stability_artifacts/` folder, and
  compare with the schema-3 app side by side: rendering, the notices, blob decoding on real shapes, the speed
  of the reads (the benchmark says 1 to 8 ms per page read; scores, projections and agreement blobs are new
  paths), and the memory of `datasets_info()`/`dataset_genes()` on the 20 to 50 thousand genes of a dataset.
- Check the WGCNA eigengene column order in the `fit_scores` blob against `factors.label` (the app names the
  columns by label in factor order; the artifact had its own names).
- Run `download_db()` once the file is on Hugging Face.
- Run the opt-in page tests (`STABILITY_DB=... devtools::test()`).

**Open questions for the app (decisions wanted):**
1. Gene-space alignment (Projection comparison) needs the full loadings; with the database copy it says so
   instead of approximating with zero-filled top genes. Keep, or store more (the full loadings are 1.8 GB as
   blobs, benchmark above)?
2. The alignment's axis ordering used Hungarian matching (`clue`); it now uses a greedy match on |coefficient|
   so that `clue` could go. Same result when the best matches are distinct; differs on ties. Restore `clue`?
3. The sidebar metric stays cosine/pearson/spearman because `factors` and `fit_summary` store stability for
   those three; `jaccard` is accepted by the matched-similarity queries (WGCNA modules). Store a Jaccard
   stability if the sidebar should offer it everywhere.
4. Projections: the app uses `include_intercept = 0` for within-dataset and `1` for cross-dataset
   projections (what the pipeline computes). The open analysis question in section 5 (within vs cross
   `relative_r2`) is unchanged.
5. `fit_summary` and `factor_association` exist for representative fits only; the sidebar's rank list is
   representative fits only, so nothing is lost today. A page that picks other fits would need more rows.
6. Biplot arrows treat a gene that is outside a factor's stored top 50 per side as zero on it (the notice says
   so). Keep, or restrict arrows to genes stored for both axes?

**Still open from before:**
- `utils::globalVariables` in `R/sepsisfactorsapp-package.R` stands in for `.data$` in the ggplot code; replace
  when the code is refactored.
- Update to schema-2 content: flags for GSE54514 (annotations do not match arrays) and sex-discordant
  samples on the time page; refresh stale strings.
- Port the remaining source `logs/dev/test_*.R` scripts (trajectory colours, projection views and scores,
  WGCNA selection, time page); shinytest2 smoke tests (not installed here).
- Look at every page in a browser against the old app, side by side.
- License, Title and Description in DESCRIPTION are placeholders (user).
- No `air format`, no pkgdown site yet.

### Payload size benchmark (2026-10-16; `results_new/bench/summary.csv`, sample per kind, ratio x total)
Estimated database growth if the whole kind is stored in a SQLite table (35 datasets). Original `.rds` size
in brackets. Samples: 50 fits (loadings, scores), 20 drivers, 100 projections, 20 dendrograms.

| Payload (rds total) | Representation | Est. size | Page read |
|---|---|---|---|
| loadings (3.8 GB, 6,991 fits) | full long table | 11.2 GB | 1 ms |
| | blob float32 gzip (xz no smaller) | 1.8 GB | 3 ms |
| | top 200 per side, long | 671 MB | 1 ms |
| | top 50 per side, long | 167 MB | 1 ms |
| | top 10 per side, long | 34 MB | 1 ms |
| scores (63 MB) | long / blob | 176 MB / 35 MB | 1 ms |
| pattern drivers PV (18 GB, 1,939) | long, all genes | 426 MB | 8 ms |
| | blob | 96 MB | 1 ms |
| | long, significant genes only | 63 MB | 1 ms |
| drivers CI (0.85 GB, 1,885) | long, all genes / top 25 | 453 MB / 2 MB | 5 ms / 0 ms |
| projections (0.72 GB, 27,710) | long with pval | 1.3 GB | 2 ms |
| | long, weight only | 885 MB | 1 ms |
| | blob (weight, pval, r2) | 336 MB | 1 ms |
| PCA diag (`mse_by_rank`, `sdev`) | blob | under 1 MB | |
| WGCNA dendrograms (78 MB) | blob | 38 MB | |

Findings: the driver `PV` files are 13 MB each because they embed ggplot objects; the app uses a few columns.
`diag` (4.2 GB) is only read for PCA. `feature_metadata` is 97 MB but the app needs only id and symbol.

### Decisions for schema 4 (2026-10-16)
- Database payloads (the smaller options): loadings top 50 per factor side (long); scores blob; projections
  blob (`projection`, `pval`, `r_squared`); pattern drivers significant genes only (PV) and top 25 (CI); PCA
  `diag` (`mse_by_rank`, `sdev`) and WGCNA dendrogram as blobs. About +0.6 GB over the schema-3 database.
- Dual mode: the app reads the full artifact when `stability_artifacts/` sits beside the database, and
  otherwise the database copy. Every page on the database copy shows a "reduced data" notice that says what
  is reduced and that the full `sepsisfactors` output (artifacts beside the database) gives more data.
- Cross-method agreement is precomputed at a grid of N (10, 25, 50, 100, 200) and the association scan
  (factor x metadata field x test) is precomputed; the app reads both and drops `similarity.R`, `clue`,
  `mclust`, and its live association scan.
- Ownership (temporary, decided 2026-10-16): `sepsisfactors` is left alone for now. The derived tables
  (agreement grid, association scan, reduced payloads) are computed in a derive step of `sepsisfactorsdb`
  that runs inside `ingest_results()` and reads the full artifacts of the results output. This is a marked
  exception to "`sepsisfactorsdb` computes nothing" (note it in both `CLAUDE.md` files) and adds `clue` and
  `mclust` to `sepsisfactorsdb` via usethis. The contract gains no tables for them. The converter in `tools/`
  only reshapes `results/` into the contract layout.
- LATER (planning/refactor): move the derive step into `sepsisfactors` (new functions beside
  `R/analysis_similarity.R`), write the tables into the results output (bump `RESULTS_OUTPUT_VERSION`),
  and have `sepsisfactorsdb` validate and load them only; then remove the exception and the two dependencies.

### Parallel database build: feasibility (2026-10-16)
Measured on this machine (local NVMe, ATTACH + `INSERT ... SELECT` of the three biggest schema-3 tables,
no secondary indexes): `match_scores` 8.8M rows in 11 s, `gene_modules` 3.9M rows in 1.9 s,
`factor_matches` 3.0M rows in 1.4 s (0.8 to 2.1 M rows/s; 1.1 GB written in about 14 s). So the final
merge of a ~3 GB database is minutes, not the bottleneck. The earlier 75 min serial load is R-side row
building, many small transactions and fsyncs on a network filesystem.
- Parallel by dataset (35 independent units): adapter, derive step, per-dataset load into a part file.
  Parallel by fit/projection/driver file inside the big datasets (MARS, GSE95233, ANEMONES) to balance load.
- Serial: cross-dataset step (needs every dataset), final merge, index creation, one copy to the final
  filesystem.
- Stage in SQLite part files (not Parquet): the merge runs inside SQLite at 1 to 2 M rows/s with no R;
  reading Parquet back into SQLite from R is slower. Build parts on node-local disk or tmpfs (128 GB RAM
  on the cluster), create secondary indexes after the merge.
- Needs the renumber-at-merge scheme, the `crew`/`crew.cluster` controller already
  implied by the targets flow, and the check that the merged database equals a serial one.
- Local limits: 12 cores, 30 GB RAM, 31 GB free disk; `results/` is a read-only squashfs mount, so
  artifacts must be symlinked, never copied. On the cluster, hard links work only within one filesystem.

## 7. TEMPORARY patches to remove at the `sepsisfactors` refactor (added 2026-10-16, stage 1)

Stage 1 of schema 4 left `sepsisfactors` untouched. To get the database to schema 4 anyway, `sepsisfactorsdb`
carries code that does not belong there (it computes, or it reads the original project's layout). Everything
below goes when `sepsisfactors` writes the contract itself. `CLAUDE.md` (root) and `sepsisfactorsdb/CLAUDE.md`
carry a marked exception to "no statistics in `sepsisfactorsdb`" that points here.

| Piece | Where | What it does | Where it moves / how it goes |
|---|---|---|---|
| Legacy adapter | `sepsisfactorsdb/R/legacy_adapter.R`: `legacy_to_results()` (exported) and its helpers `legacy_dataset()`, `legacy_cross()`, `legacy_place()`, `resolve_legacy_mode()`, `read_legacy_config()`, `legacy_dataset_config()`, `write_legacy_table()`; tests `test-legacy_adapter.R`, `helper-legacy.R` | Reshapes the original schema-3 database + `stability_artifacts/` into a results output (v2), with family, `hf_collection` and composition markers from a `sepsisfactors/inst` config directory | Delete. `sepsisfactors` writes the v2 results output (below), so nothing reads the old layout. |
| Derive step | `sepsisfactorsdb/R/derive.R` (`derive_dataset()`, `derive_cross_dataset()`, `encode_f32()`, `encode_i32()`, `decode_*()`, `insert_columns()`, context helpers), `derive_payloads.R` (`derive_genes()`, `derive_sample_metadata()`, `derive_factor_top_genes()`, `derive_fit_scores()`, `derive_fit_diag()`, `derive_wgcna_dendrograms()`, `derive_driver_genes()`/`reduce_driver()`, `derive_gene_variance()`), `derive_summaries.R` (`derive_fit_summary()`, `derive_factor_association()`, `association_scan()`), `derive_agreement.R` (`derive_method_agreement()`, `jaccard_sets()`, ...); tests `test-derive*.R` | Builds the reduced and precomputed tables from the full artifacts at load time: `genes`, `factor_top_genes`, `fit_scores`, `projection_values`, `driver_genes`, `fit_diag`, `wgcna_dendrograms`, `sample_metadata`, `gene_variance`, `fit_summary`, `factor_association`, `method_agreement` | Move the computation into `sepsisfactors` (beside `R/analysis_similarity.R`), write these as results-output tables (new `RESULTS_OUTPUT_VERSION`), and have `sepsisfactorsdb` validate and load them only, like every other table. Then delete `R/derive*.R`, the `derive_*()` calls in `ingest_results()` and `ingest_targets()`, the chunking of the derive step in the parallel build (`plan_chunks()`, `build_chunk_part()`, `CHUNK_TABLES` in `R/parts.R`, the `chunk_*` targets and the `level`/`fit_keys` arguments of `derive_dataset()`), and the exception in both `CLAUDE.md` files. |
| Copied matching helper | `sepsisfactorsdb/R/derive_similarity.R`: `hungarian_match_abs()` (the derive step also re-implements the app's gene-set Jaccard and sample ARI in `derive_agreement.R`, and copies `association_scan()` from `sepsisfactorsapp/R/data_helpers.R`) | Hungarian matching of factors on \|Jaccard\| | `sepsisfactors/R/analysis_similarity.R` already has it; use it there. |
| Packages to remove from `sepsisfactorsdb` | `DESCRIPTION` Imports: `clue` (matching), `mclust` (ARI), and `yaml` once the adapter goes (`ingest.yml` reading still needs it, so it stays unless that moves). `arrow`, `jsonlite`, `crew` and `crew.cluster` (the parallel build) stay. | | `usethis::use_package()` added them; remove them with the files above (edit `DESCRIPTION` only through usethis or by deleting the last user). The app drops `clue` and `mclust` when it reads the precomputed agreement, and `yaml` when it stops reading config files. |
| Contract catch-up for the `sepsisfactors` writer | `RESULTS_OUTPUT_VERSION` 2 in `sepsisfactorsdb/R/schema.R` | `sepsisfactors` still writes version 1. It must: write the optional `datasets` columns `family`, `family_position`, `hf_collection` (from `dataset_families.yml` and each dataset config), the `composition_markers` cross table, and the `dataset_artifacts` kinds `sample_metadata`, `feature_metadata`, `variance_partition` as the loader reads them; it may stop writing columns the database no longer stores (the "extra" columns of `RESULT_TABLES`) and the tables the database no longer holds (`similarity_histograms`, `icasso_membership`, `fit_redundancy`, `factor_variance_models`, `factor_variance`), though the contract still accepts them. Gene ids in loadings, drivers, dendrograms and variance partitions must be ids of the dataset's `feature_metadata`. | Done when the writer's output passes `validate_results()` at version 2 (or the next version, once the derived tables move). |
| Artifact mode `"symlink"` | `place_artifacts()` and `ingest_results(artifact_mode = )` | Added because the legacy tree sits on a read-only squashfs on another filesystem; hard links cannot cross it and copying 28 GB would fill the disk. | Keep (harmless), or drop with the adapter if nothing else needs it. |
| Development scripts | `sepsisfactorsdb/dev/smoke_schema4.R`, `dev/parity_schema4.R` (build-ignored) | Smoke run of the adapter and loader on a legacy dataset; parity of `fit_summary`, `factor_association` and `method_agreement` with the app's live functions | Delete with the adapter. |

Full run of the pipeline (not run in stage 1; needs the whole `results/`, about 2-2.5 h serial on a local disk,
estimated from the single-dataset smoke runs: loading about 2 min per dataset, derive about 1 min for a
dataset like `GSE110487_T1`, driver reduction about 0.5 s per driver file):

```sh
Rscript -e 'devtools::load_all("sepsisfactorsdb"); legacy_to_results("results", "results_new/results", config_dir = "sepsisfactors/inst", artifact_mode = "symlink"); ingest_results("results_new/results", "results_new/stability.sqlite", artifact_mode = "symlink")'
```
