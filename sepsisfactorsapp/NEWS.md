# sepsisfactorsapp 0.0.0.9000

* The app now reads schema version 4 of the database and refuses any other. It
  runs from the database file alone; an optional `stability_artifacts/` folder
  beside it adds the full loadings, scores, projections and pattern-driver
  results. Pages that fall back to the reduced copy in the database say what is
  reduced ("Reduced data" notice): the biplot gene arrows, the Welch-test pattern
  drivers, and the gene-space alignment, which is unavailable without the full
  loadings.
* `download_db()` downloads the published database (without the ROSE datasets)
  into the working directory, checks that it is an SQLite file at the expected
  schema version, and keeps an existing file unless `overwrite = TRUE`.
* `run_app()` and `app()` lose the `config_dir` argument, the
  `SEPSISFACTORSAPP_CONFIG` variable and the YAML files: dataset families and
  timepoint order, composition markers and documentation links come from the
  database, as do method labels, colours, order and descriptions.
* Cross-method agreement, the factor-by-metadata association scan and the fit
  quality/stability curves are read from precomputed tables instead of being
  computed live. The top-genes choice on the Compare page is the grid the
  database stored (10, 25, 50, 100, 200). The copied matching helpers
  (`similarity.R`) and the `clue`, `mclust` and `yaml` dependencies are gone; the
  gene-space alignment orders its axes by a greedy match on |coefficient|.
* Projections are filtered on `include_intercept` explicitly (within-dataset
  without intercept, cross-dataset with it). `jaccard` is accepted wherever
  matched similarities are read.
* All SQL is in `db_*.R`, with bound parameters; the connection is a handle passed
  to the functions (no `options(stability.db_dir)`, no cached environments). A
  missing table is a classed condition turned into a notice.
