test_that("example results load completely and consistently", {
  root <- local_results()
  db <- local_db()
  res <- ingest_results(root, db)
  expect_equal(res$datasets$TOY1$fits[["ok"]], 12)
  expect_equal(res$datasets$TOY1$fits[["failed"]], 1)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  expect_equal(nrow(DBI::dbGetQuery(con, "PRAGMA foreign_key_check")), 0)
  expect_equal(db_schema_version(con), SCHEMA_VERSION)
  expect_equal(db_count(db, "datasets"), 2)
  expect_equal(db_count(db, "fits"), 14)
  expect_equal(db_count(db, "projections"), 1)
  expect_equal(db_count(db, "gene_space_agreement"), 1)
  expect_equal(db_count(db, "enrichment_results"), 2)
  expect_equal(db_count(db, "projection_values"), 1)
  expect_equal(db_count(db, "composition_markers"), 3)
  # the derive step ran for every table it owns
  for (table in c(
    "genes", "factor_top_genes", "fit_scores", "fit_diag", "wgcna_dendrograms", "driver_genes",
    "sample_metadata", "fit_summary", "factor_association", "method_agreement"
  )) {
    expect_gt(db_count(db, table), 0)
  }

  # a pair is stored with fit_a < fit_b and its factors swapped with it
  pairs <- db_query(db, "SELECT fit_a, fit_b FROM fit_pairs")
  expect_true(all(pairs$fit_a < pairs$fit_b))
  m <- db_query(db, "
    SELECT fa.seed AS seed_a, xa.factor_index AS idx_a, fb.seed AS seed_b, xb.factor_index AS idx_b
    FROM factor_matches fm
    JOIN fit_pairs fp ON fp.fit_pair_id = fm.fit_pair_id
    JOIN fits fa ON fa.fit_id = fp.fit_a JOIN fits fb ON fb.fit_id = fp.fit_b
    JOIN factors xa ON xa.factor_id = fm.factor_a JOIN factors xb ON xb.factor_id = fm.factor_b
    ORDER BY idx_a")
  expect_equal(m$seed_a, c(1, 1))
  expect_equal(m$seed_b, c(2, 2))
  expect_equal(m$idx_b, 2:1)

  # artifact paths are relative to the database; only the pointer artifacts are placed
  paths <- db_query(db, "SELECT path FROM fit_artifacts UNION SELECT path FROM projections
                         UNION SELECT path FROM pattern_drivers")$path
  expect_true(all(startsWith(paths, "stability_artifacts/")))
  expect_true(all(file.exists(resolve_artifact(paths, db))))
  placed <- list.files(artifacts_dir(db, "TOY1"))
  expect_false(any(grepl("_diag\\.rds$|dendrogram|matrix|metadata", placed)))
  expect_true(any(grepl("_loadings\\.rds$", placed)))

  # provenance, method display metadata and the optional dataset columns
  meta <- db_query(db, "SELECT key, value FROM meta")
  expect_setequal(meta$key, c("schema_version", "created_at", "writer", "writer_version"))
  methods <- db_query(db, "SELECT * FROM methods ORDER BY display_order")
  expect_equal(methods$method, c("pca", "spca", "ica", "nmf", "cogaps", "wgcna"))
  d <- db_query(db, "SELECT family, hf_collection FROM datasets WHERE dataset_id = 'TOY1'")
  expect_equal(d$family, "TOY1")
  expect_equal(d$hf_collection, "toy/collection")
})

test_that("projections onto a dataset that is not loaded are refused", {
  root <- local_results()
  db <- local_db()
  load_dataset(root, db, "TOY1")
  expect_snapshot(load_cross_dataset(root, db), error = TRUE, transform = scrub_tmp)
})

test_that("invalid results are rejected before anything is written", {
  root <- local_results()
  edit_table(root, "TOY2", "fit_params", function(df) df[, c("fit_key", "name")])
  db <- local_db()
  expect_error(ingest_results(root, db), class = "sepsisfactorsdb_invalid_results")
  expect_false(file.exists(db))
})
