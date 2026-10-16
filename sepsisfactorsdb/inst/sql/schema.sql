-- Stability DB schema (SCHEMA_VERSION 4).
-- Created by open_stability_db() on a new database, which then stamps
-- PRAGMA user_version with SCHEMA_VERSION. There are no migrations: the DB is
-- rebuilt from the results output.
--
-- Statements are separated by a line holding only ";;" (read_sql_statements()
-- splits on it), since triggers or CHECKs may contain semicolons.

-- ---- provenance and dataset level ----------------------------------------

CREATE TABLE meta (
  key   TEXT PRIMARY KEY,
  value TEXT NOT NULL
)
;;
CREATE TABLE datasets (
  dataset_id      TEXT PRIMARY KEY,
  sample_id_col   TEXT,
  feature_id_col  TEXT,
  symbol_col      TEXT,
  subject_id_col  TEXT,
  family          TEXT,
  family_position INTEGER,
  hf_collection   TEXT
)
;;
CREATE TABLE matrix_diagnostics (
  dataset_id TEXT PRIMARY KEY REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  val_sd     REAL
)
;;
CREATE TABLE genes (
  dataset_id TEXT NOT NULL REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  gene_idx   INTEGER NOT NULL,
  gene_id    TEXT NOT NULL,
  symbol     TEXT,
  PRIMARY KEY (dataset_id, gene_idx)
) WITHOUT ROWID
;;
CREATE UNIQUE INDEX idx_genes_id ON genes(dataset_id, gene_id)
;;

-- ---- methods and fits ------------------------------------------------------

CREATE TABLE methods (
  method         TEXT PRIMARY KEY,
  family         TEXT NOT NULL,
  sign_ambiguous INTEGER NOT NULL CHECK (sign_ambiguous IN (0, 1)),
  has_loadings   INTEGER NOT NULL CHECK (has_loadings IN (0, 1)),
  label          TEXT NOT NULL,
  color          TEXT NOT NULL,
  display_order  INTEGER NOT NULL,
  blurb          TEXT NOT NULL
)
;;
CREATE TABLE fits (
  fit_id         INTEGER PRIMARY KEY,
  fit_key        TEXT NOT NULL UNIQUE,
  dataset_id     TEXT NOT NULL REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  method         TEXT NOT NULL REFERENCES methods(method),
  rank           INTEGER,
  seed           INTEGER,
  bootstrap      INTEGER NOT NULL DEFAULT 0 CHECK (bootstrap IN (0, 1)),
  representative INTEGER NOT NULL DEFAULT 0 CHECK (representative IN (0, 1)),
  mse            REAL,
  n_factors      INTEGER,
  status         TEXT NOT NULL CHECK (status IN ('ok', 'failed'))
)
;;
CREATE INDEX idx_fits_dataset_method_rank ON fits(dataset_id, method, rank)
;;
CREATE TABLE fit_params (
  fit_id     INTEGER NOT NULL REFERENCES fits(fit_id) ON DELETE CASCADE,
  name       TEXT NOT NULL,
  num_value  REAL,
  text_value TEXT,
  PRIMARY KEY (fit_id, name)
)
;;
CREATE TABLE fit_metrics (
  fit_id INTEGER NOT NULL REFERENCES fits(fit_id) ON DELETE CASCADE,
  name   TEXT NOT NULL,
  value  REAL,
  PRIMARY KEY (fit_id, name)
)
;;
-- Pointers to the full artifacts (kinds 'loadings' and 'scores'); optional at
-- app run time, the reduced copies are fit_scores and factor_top_genes.
CREATE TABLE fit_artifacts (
  fit_id INTEGER NOT NULL REFERENCES fits(fit_id) ON DELETE CASCADE,
  kind   TEXT NOT NULL,
  path   TEXT NOT NULL,
  PRIMARY KEY (fit_id, kind)
)
;;
CREATE TABLE factors (
  factor_id          INTEGER PRIMARY KEY,
  fit_id             INTEGER NOT NULL REFERENCES fits(fit_id) ON DELETE CASCADE,
  factor_index       INTEGER NOT NULL,
  label              TEXT,
  n_genes            INTEGER,
  stability_cosine   REAL,
  stability_pearson  REAL,
  stability_spearman REAL,
  kurtosis           REAL,
  excess_kurtosis    REAL,
  UNIQUE (fit_id, factor_index)
)
;;

-- ---- pair stability --------------------------------------------------------

CREATE TABLE fit_pairs (
  fit_pair_id INTEGER PRIMARY KEY,
  fit_a       INTEGER NOT NULL REFERENCES fits(fit_id) ON DELETE CASCADE,
  fit_b       INTEGER NOT NULL REFERENCES fits(fit_id) ON DELETE CASCADE,
  same_rank   INTEGER NOT NULL CHECK (same_rank IN (0, 1)),
  ari         REAL,
  UNIQUE (fit_a, fit_b),
  CHECK (fit_a < fit_b)
)
;;
CREATE INDEX idx_fit_pairs_b ON fit_pairs(fit_b)
;;
CREATE TABLE factor_matches (
  match_id    INTEGER PRIMARY KEY,
  fit_pair_id INTEGER NOT NULL REFERENCES fit_pairs(fit_pair_id) ON DELETE CASCADE,
  factor_a    INTEGER NOT NULL REFERENCES factors(factor_id) ON DELETE CASCADE,
  factor_b    INTEGER NOT NULL REFERENCES factors(factor_id) ON DELETE CASCADE,
  UNIQUE (fit_pair_id, factor_a)
)
;;
CREATE INDEX idx_factor_matches_a ON factor_matches(factor_a)
;;
CREATE INDEX idx_factor_matches_b ON factor_matches(factor_b)
;;
CREATE TABLE match_scores (
  match_id  INTEGER NOT NULL REFERENCES factor_matches(match_id) ON DELETE CASCADE,
  metric    TEXT NOT NULL CHECK (metric IN ('cosine', 'pearson', 'spearman', 'jaccard')),
  value     REAL NOT NULL,
  runner_up REAL,
  margin    REAL,
  PRIMARY KEY (match_id, metric)
)
;;
CREATE TABLE fit_pair_null (
  fit_pair_id INTEGER NOT NULL REFERENCES fit_pairs(fit_pair_id) ON DELETE CASCADE,
  metric      TEXT NOT NULL CHECK (metric IN ('cosine', 'pearson', 'spearman', 'jaccard')),
  p95         REAL,
  PRIMARY KEY (fit_pair_id, metric)
)
;;

-- ---- WGCNA -----------------------------------------------------------------

CREATE TABLE gene_modules (
  fit_id      INTEGER NOT NULL REFERENCES fits(fit_id) ON DELETE CASCADE,
  gene        TEXT NOT NULL,
  module      INTEGER NOT NULL,
  kme_own     REAL,
  next_module INTEGER,
  kme_next    REAL,
  margin      REAL,
  p_analytic  REAL,
  p_perm      REAL,
  PRIMARY KEY (fit_id, gene)
)
;;
CREATE INDEX idx_gene_modules_module ON gene_modules(fit_id, module)
;;
CREATE TABLE module_kme_summary (
  fit_id INTEGER NOT NULL REFERENCES fits(fit_id) ON DELETE CASCADE,
  module INTEGER NOT NULL,
  grp    TEXT NOT NULL CHECK (grp IN ('member', 'nonmember', 'permutation')),
  n      INTEGER NOT NULL,
  mean   REAL,
  median REAL,
  p95    REAL,
  PRIMARY KEY (fit_id, module, grp)
)
;;
CREATE TABLE module_kme_histograms (
  fit_id INTEGER NOT NULL REFERENCES fits(fit_id) ON DELETE CASCADE,
  module INTEGER NOT NULL,
  grp    TEXT NOT NULL CHECK (grp IN ('member', 'nonmember', 'permutation')),
  bin    INTEGER NOT NULL,
  bin_lo REAL NOT NULL,
  bin_hi REAL NOT NULL,
  count  INTEGER NOT NULL,
  PRIMARY KEY (fit_id, module, grp, bin)
)
;;
CREATE TABLE wgcna_sft (
  dataset_id     TEXT NOT NULL REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  power          INTEGER NOT NULL,
  sft_r_sq       REAL,
  PRIMARY KEY (dataset_id, power)
)
;;
CREATE TABLE gene_significance (
  dataset_id TEXT NOT NULL REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  field      TEXT NOT NULL,
  gene       TEXT NOT NULL,
  test       TEXT NOT NULL CHECK (test IN ('spearman', 'kruskal')),
  statistic  REAL,
  p_value    REAL,
  PRIMARY KEY (dataset_id, field, gene)
)
;;

-- ---- ICA / ICASSO ----------------------------------------------------------

CREATE TABLE icasso_clusters (
  dataset_id TEXT NOT NULL REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  rank       INTEGER NOT NULL,
  cluster_id INTEGER NOT NULL,
  iq         REAL,
  n_members  INTEGER NOT NULL,
  PRIMARY KEY (dataset_id, rank, cluster_id)
)
;;

-- ---- enrichment ------------------------------------------------------------

CREATE TABLE enrichment_queries (
  query_id   INTEGER PRIMARY KEY,
  factor_id  INTEGER NOT NULL REFERENCES factors(factor_id) ON DELETE CASCADE,
  query_type TEXT NOT NULL,
  direction  TEXT NOT NULL CHECK (direction IN ('pos', 'neg', 'both')),
  UNIQUE (factor_id, query_type, direction)
)
;;
CREATE TABLE enrichment_results (
  query_id          INTEGER NOT NULL REFERENCES enrichment_queries(query_id) ON DELETE CASCADE,
  term_id           TEXT NOT NULL,
  source            TEXT NOT NULL,
  term_name         TEXT,
  p_value           REAL NOT NULL,
  intersection_size INTEGER,
  term_size         INTEGER,
  nes               REAL,
  PRIMARY KEY (query_id, term_id)
)
;;
CREATE INDEX idx_enrichment_results_p ON enrichment_results(query_id, p_value)
;;

-- ---- cross-dataset ---------------------------------------------------------

CREATE TABLE projections (
  projection_id     INTEGER PRIMARY KEY,
  source_fit_id     INTEGER NOT NULL REFERENCES fits(fit_id) ON DELETE CASCADE,
  target_dataset_id TEXT NOT NULL REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  projection_type   TEXT NOT NULL CHECK (projection_type IN ('within_dataset', 'cross_dataset')),
  include_intercept INTEGER NOT NULL CHECK (include_intercept IN (0, 1)),
  relative_r2       REAL,
  path              TEXT NOT NULL,
  UNIQUE (source_fit_id, target_dataset_id, include_intercept)
)
;;
CREATE INDEX idx_projections_target ON projections(target_dataset_id)
;;
CREATE TABLE gene_space_agreement (
  source_fit_id     INTEGER NOT NULL REFERENCES fits(fit_id) ON DELETE CASCADE,
  target_fit_id     INTEGER NOT NULL REFERENCES fits(fit_id) ON DELETE CASCADE,
  n_genes_matched   INTEGER NOT NULL,
  mean_abs_diagonal REAL NOT NULL,
  PRIMARY KEY (source_fit_id, target_fit_id)
)
;;

-- ---- representative-fit analyses -------------------------------------------

CREATE TABLE pattern_drivers (
  driver_id    INTEGER PRIMARY KEY,
  factor_id    INTEGER NOT NULL REFERENCES factors(factor_id) ON DELETE CASCADE,
  grouping_col TEXT NOT NULL,
  group1_level TEXT NOT NULL,
  group2_level TEXT NOT NULL,
  mode         TEXT NOT NULL CHECK (mode IN ('CI', 'PV')),
  path         TEXT NOT NULL,
  UNIQUE (factor_id, grouping_col, group1_level, group2_level, mode)
)
;;

-- ---- repeated measures: design, sample QC, variance partition ---------------
-- Written for datasets with a `design:` block and, from the parent's design,
-- for their per-timepoint subsets (R/targets/variance.R, variance_explore.R,
-- composition.R).

CREATE TABLE dataset_design (
  dataset_id           TEXT PRIMARY KEY REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  parent_dataset_id    TEXT NOT NULL,
  is_subset            INTEGER NOT NULL CHECK (is_subset IN (0, 1)),
  platform             TEXT,
  subject_col          TEXT,
  time_col             TEXT,
  time_order           TEXT,
  group_col            TEXT,
  covariates           TEXT,
  technical_covariates TEXT,
  vp_formula           TEXT,
  dream_formula        TEXT,
  n_samples            INTEGER,
  n_subjects           INTEGER,
  n_dropped            INTEGER
)
;;
CREATE TABLE samples (
  dataset_id       TEXT NOT NULL REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  sample_id        TEXT NOT NULL,
  subject          TEXT,
  timepoint        TEXT,
  grp              TEXT,
  cell             TEXT,
  pc1              REAL,
  pc2              REAL,
  pc3              REAL,
  neutrophil_score REAL,
  lymphocyte_score REAL,
  nl_score         REAL,
  chrY_score       REAL,
  xist             REAL,
  inferred_sex     TEXT,
  recorded_sex     TEXT,
  PRIMARY KEY (dataset_id, sample_id)
)
;;
CREATE TABLE variance_summary (
  dataset_id                      TEXT PRIMARY KEY REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  n_genes                         INTEGER,
  n_genes_failed                  INTEGER,
  share_singular                  REAL,
  max_colinearity                 REAL,
  dupcor_consensus                REAL,
  between_median                  REAL,
  time_median                     REAL,
  residual_median                 REAL,
  technical_median                REAL,
  repeatability_median            REAL,
  time_within_median              REAL,
  share_time_gt10                 REAL,
  n_subjects_repeated             INTEGER,
  n_changes                       INTEGER,
  change_pc1_share                REAL,
  change_pc1_cor_nl               REAL,
  median_r2_nl_factor_genes       REAL,
  median_r2_technical_factor_genes REAL,
  rho_pc1_nl                      REAL,
  control_factor_fraction         REAL,
  sex_separation                  REAL,
  sex_agreement                   REAL,
  n_subjects_sex_discordant       INTEGER,
  nl_subject_fraction             REAL,
  nl_time_fraction                REAL
)
;;
CREATE TABLE variance_terms (
  dataset_id TEXT NOT NULL REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  term       TEXT NOT NULL,
  category   TEXT NOT NULL,
  median     REAL,
  q90        REAL,
  share_gt05 REAL,
  share_gt10 REAL,
  PRIMARY KEY (dataset_id, term)
)
;;
CREATE TABLE variance_categories (
  dataset_id      TEXT NOT NULL REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  category        TEXT NOT NULL,
  present         INTEGER NOT NULL CHECK (present IN (0, 1)),
  mean            REAL,
  median          REAL,
  vw_all          REAL,
  vw_factor_genes REAL,
  vw_top          REAL,
  PRIMARY KEY (dataset_id, category)
)
;;
CREATE TABLE variance_bins (
  dataset_id TEXT NOT NULL REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  binning    TEXT NOT NULL CHECK (binning IN ('expression_quartile', 'variance_rank')),
  bin        TEXT NOT NULL,
  n_genes         INTEGER,
  between_subject REAL,
  time            REAL,
  residual        REAL,
  PRIMARY KEY (dataset_id, binning, bin)
)
;;
CREATE TABLE trait_variance (
  dataset_id TEXT NOT NULL REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  trait      TEXT NOT NULL,
  term       TEXT NOT NULL,
  category   TEXT NOT NULL,
  fraction   REAL,
  PRIMARY KEY (dataset_id, trait, term)
)
;;
CREATE TABLE composition_correlations (
  dataset_id TEXT NOT NULL REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  x          TEXT NOT NULL,
  y          TEXT NOT NULL,
  rho        REAL,
  p_value    REAL,
  PRIMARY KEY (dataset_id, x, y)
)
;;
CREATE TABLE de_contrasts (
  dataset_id TEXT NOT NULL REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  contrast   TEXT NOT NULL,
  kind       TEXT NOT NULL CHECK (kind IN ('pairwise', 'omnibus')),
  readable   TEXT,
  n_genes    INTEGER,
  n_fdr05    INTEGER,
  PRIMARY KEY (dataset_id, contrast)
)
;;
CREATE TABLE variance_time_genes (
  dataset_id TEXT NOT NULL REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  symbol          TEXT NOT NULL,
  time_all        REAL,
  between_subject REAL,
  PRIMARY KEY (dataset_id, symbol)
)
;;
CREATE TABLE time_gene_recurrence (
  symbol     TEXT PRIMARY KEY,
  n_datasets INTEGER NOT NULL,
  datasets   TEXT NOT NULL
)
;;
CREATE TABLE composition_markers (
  marker_set TEXT NOT NULL,
  gene       TEXT NOT NULL,
  PRIMARY KEY (marker_set, gene)
) WITHOUT ROWID
;;

-- ---- derived tables ----------------------------------------------------------
-- Built by the derive step (R/derive_*.R) from the full artifacts of the
-- results output, so the app can run from this file alone. Blobs are float32
-- (or int32) values, column-major, compressed with memCompress(type = "gzip").

-- Top genes of each factor on each side of zero. Reduced copy of the loadings.
CREATE TABLE factor_top_genes (
  fit_id       INTEGER NOT NULL REFERENCES fits(fit_id) ON DELETE CASCADE,
  factor_index INTEGER NOT NULL,
  side         TEXT NOT NULL CHECK (side IN ('pos', 'neg')),
  rank         INTEGER NOT NULL,
  gene_idx     INTEGER NOT NULL,
  loading      REAL NOT NULL,
  PRIMARY KEY (fit_id, factor_index, side, rank)
) WITHOUT ROWID
;;
-- Sample scores of a fit (module eigengenes for WGCNA): samples x n_factors.
CREATE TABLE fit_scores (
  fit_id    INTEGER PRIMARY KEY REFERENCES fits(fit_id) ON DELETE CASCADE,
  row_ids   TEXT NOT NULL,
  n_factors INTEGER NOT NULL,
  data      BLOB NOT NULL
)
;;
-- A projection result: components x samples weights, then their p values
-- (same shape), then r_squared (one per sample).
CREATE TABLE projection_values (
  projection_id INTEGER PRIMARY KEY REFERENCES projections(projection_id) ON DELETE CASCADE,
  row_ids       TEXT NOT NULL,
  col_ids       TEXT NOT NULL,
  data          BLOB NOT NULL
)
;;
-- Reduced projectionDriveR result. PV: genes that are shared-significant or
-- have Welch adjusted p < 0.05 (mean_diff, padj, weight, shared). CI: the
-- top 25 genes by |low + high| plus the shared genes (low, high, weight, shared).
CREATE TABLE driver_genes (
  driver_id INTEGER NOT NULL REFERENCES pattern_drivers(driver_id) ON DELETE CASCADE,
  gene_idx  INTEGER NOT NULL,
  mean_diff REAL,
  padj      REAL,
  weight    REAL,
  shared    INTEGER NOT NULL CHECK (shared IN (0, 1)),
  low       REAL,
  high      REAL,
  PRIMARY KEY (driver_id, gene_idx)
) WITHOUT ROWID
;;
-- PCA only: mse_by_rank and sdev, one row per rank.
CREATE TABLE fit_diag (
  fit_id INTEGER NOT NULL REFERENCES fits(fit_id) ON DELETE CASCADE,
  name   TEXT NOT NULL,
  idx    INTEGER NOT NULL,
  value  REAL,
  PRIMARY KEY (fit_id, name, idx)
) WITHOUT ROWID
;;
-- The hclust pieces of a WGCNA gene dendrogram; gene_idx are the leaf labels.
CREATE TABLE wgcna_dendrograms (
  fit_id   INTEGER PRIMARY KEY REFERENCES fits(fit_id) ON DELETE CASCADE,
  n        INTEGER NOT NULL,
  merge    BLOB NOT NULL,
  height   BLOB NOT NULL,
  ord      BLOB NOT NULL,
  gene_idx BLOB NOT NULL
)
;;
-- A dataset's sample metadata, long. type is the R class, so a reader can
-- rebuild the data frame (factor levels are not kept).
CREATE TABLE sample_metadata (
  dataset_id TEXT NOT NULL REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  sample_id  TEXT NOT NULL,
  field      TEXT NOT NULL,
  type       TEXT NOT NULL,
  value      TEXT,
  PRIMARY KEY (dataset_id, sample_id, field)
) WITHOUT ROWID
;;
CREATE TABLE gene_variance (
  dataset_id  TEXT NOT NULL REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  gene_idx    INTEGER NOT NULL,
  time_all    REAL,
  factor_gene INTEGER CHECK (factor_gene IN (0, 1)),
  PRIMARY KEY (dataset_id, gene_idx)
) WITHOUT ROWID
;;
-- One row per representative fit: the quality and stability the Compare page plots.
CREATE TABLE fit_summary (
  fit_id             INTEGER PRIMARY KEY REFERENCES fits(fit_id) ON DELETE CASCADE,
  in_sample_r2       REAL,
  stability_cosine   REAL,
  stability_pearson  REAL,
  stability_spearman REAL
)
;;
-- Cross-method agreement of two representative fits (PCA fits are used at
-- n_factors components): mean matched gene-set Jaccard at top_n genes per
-- factor, sample-cluster ARI, and the matched factors.
CREATE TABLE method_agreement (
  agreement_id   INTEGER PRIMARY KEY,
  dataset_id     TEXT NOT NULL REFERENCES datasets(dataset_id) ON DELETE CASCADE,
  fit_a          INTEGER NOT NULL REFERENCES fits(fit_id) ON DELETE CASCADE,
  n_factors_a    INTEGER NOT NULL,
  fit_b          INTEGER NOT NULL REFERENCES fits(fit_id) ON DELETE CASCADE,
  n_factors_b    INTEGER NOT NULL,
  top_n          INTEGER NOT NULL,
  gene_jaccard   REAL,
  sample_ari     REAL,
  n_shared       INTEGER,
  jaccard_matrix BLOB,
  matches        BLOB,
  UNIQUE (fit_a, n_factors_a, fit_b, n_factors_b, top_n)
)
;;
CREATE INDEX idx_method_agreement_dataset ON method_agreement(dataset_id, top_n)
;;
-- Association of each factor's scores with each sample-metadata field of a
-- representative fit (BH over the fit's grid).
CREATE TABLE factor_association (
  fit_id       INTEGER NOT NULL REFERENCES fits(fit_id) ON DELETE CASCADE,
  factor_index INTEGER NOT NULL,
  field        TEXT NOT NULL,
  test         TEXT NOT NULL CHECK (test IN ('spearman', 'kruskal')),
  statistic    REAL,
  p_value      REAL,
  p_adj        REAL,
  PRIMARY KEY (fit_id, factor_index, field)
) WITHOUT ROWID
