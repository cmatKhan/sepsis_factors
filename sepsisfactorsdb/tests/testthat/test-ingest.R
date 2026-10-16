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
  expect_equal(db_count(db, "methods"), 6)
  expect_equal(db_count(db, "projections"), 1)
  expect_equal(db_count(db, "gene_space_agreement"), 1)
  expect_equal(db_count(db, "enrichment_results"), 2)
  expect_equal(db_count(db, "icasso_membership"), 6)
})

test_that("pairs are stored with fit_a < fit_b and their factors swapped with them", {
  root <- local_results()
  db <- local_db()
  ingest_results(root, db)
  pairs <- db_query(db, "SELECT fit_a, fit_b FROM fit_pairs")
  expect_true(all(pairs$fit_a < pairs$fit_b))
  # the ICA pair was given as (seed 2, seed 1) with factor 1 <-> 2, 2 <-> 1
  m <- db_query(db, "
    SELECT fa.seed AS seed_a, xa.factor_index AS idx_a, fb.seed AS seed_b, xb.factor_index AS idx_b
    FROM factor_matches fm
    JOIN fit_pairs fp ON fp.fit_pair_id = fm.fit_pair_id
    JOIN fits fa ON fa.fit_id = fp.fit_a JOIN fits fb ON fb.fit_id = fp.fit_b
    JOIN factors xa ON xa.factor_id = fm.factor_a JOIN factors xb ON xb.factor_id = fm.factor_b
    ORDER BY idx_a")
  expect_equal(m$seed_a, c(1, 1))
  expect_equal(m$seed_b, c(2, 2))
  expect_equal(m$idx_a, 1:2)
  expect_equal(m$idx_b, 2:1)
  expect_equal(db_count(db, "match_scores"), 6)
})

test_that("artifact paths are stored relative to the database and files exist", {
  root <- local_results()
  db <- local_db()
  ingest_results(root, db)
  paths <- db_query(db, "SELECT path FROM fit_artifacts UNION SELECT path FROM dataset_artifacts
                         UNION SELECT path FROM projections UNION SELECT path FROM pattern_drivers
                         UNION SELECT dendro_file FROM icasso_clusters")$path
  expect_true(all(startsWith(paths, "stability_artifacts/")))
  expect_true(all(file.exists(resolve_artifact(paths, db))))
  # projections live with their source dataset
  expect_match(db_query(db, "SELECT path FROM projections")$path, "^stability_artifacts/TOY1/")
})

test_that("loading twice is idempotent and keeps fit ids", {
  root <- local_results()
  db <- local_db()
  ingest_results(root, db)
  before <- db_query(db, "SELECT fit_id, fit_key FROM fits ORDER BY fit_key")
  counts <- vapply(c("fits", "factors", "fit_pairs", "factor_matches", "enrichment_queries"), db_count, numeric(1), db = db)
  ingest_results(root, db)
  expect_equal(db_query(db, "SELECT fit_id, fit_key FROM fits ORDER BY fit_key"), before)
  expect_equal(vapply(names(counts), db_count, numeric(1), db = db), counts)
})

test_that("a fit dropped from the results is dropped from the database", {
  root <- local_results()
  db <- local_db()
  ingest_results(root, db)
  drop <- db_query(db, "SELECT fit_key FROM fits WHERE method = 'nmf' AND seed = 2")$fit_key
  for (table in c("fits", "factors", "fit_params", "fit_metrics", "fit_artifacts")) {
    edit_table(root, "TOY1", table, function(df) df[df$fit_key != drop, ])
  }
  ingest_results(root, db, "TOY1")
  expect_equal(db_count(db, "fits"), 13)
  expect_equal(nrow(db_query(db, sprintf("SELECT * FROM fits WHERE fit_key = '%s'", drop))), 0)
  # its artifact files are orphans and go at the next cross-dataset load
  expect_false(any(grepl(drop, list.files(artifacts_dir(db, "TOY1")))))
})

test_that("artifact modes all work and move empties the results", {
  for (mode in c("link", "copy", "move")) {
    root <- local_results()
    db <- local_db()
    ingest_results(root, db, artifact_mode = mode)
    expect_true(file.exists(file.path(artifacts_dir(db, "TOY1"), "matrix.rds")))
    expect_equal(file.exists(file.path(root, "datasets", "TOY1", "artifacts", "matrix.rds")), mode != "move")
  }
})

test_that("orphan cleanup removes unreferenced files in the database tree only", {
  root <- local_results()
  db <- local_db()
  ingest_results(root, db)
  stray <- file.path(artifacts_dir(db, "TOY1"), "stray.rds")
  saveRDS(1, stray)
  ingest_results(root, db)
  expect_false(file.exists(stray))
  expect_true(file.exists(file.path(root, "datasets", "TOY1", "artifacts", "matrix.rds")))
})

test_that("projections onto a dataset that is not loaded are refused", {
  root <- local_results()
  db <- local_db()
  load_dataset(root, db, "TOY1")
  expect_snapshot(load_cross_dataset(root, db), error = TRUE, transform = scrub_tmp)
})

test_that("results without cross-dataset tables load without them", {
  root <- local_results()
  unlink(file.path(root, "cross_dataset"), recursive = TRUE)
  db <- local_db()
  res <- ingest_results(root, db)
  expect_null(res$cross_dataset)
  expect_equal(db_count(db, "projections"), 0)
})

test_that("a database of another schema version is not written to", {
  root <- local_results()
  db <- local_db()
  ingest_results(root, db)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, "PRAGMA user_version = 99")
  DBI::dbDisconnect(con)
  expect_error(ingest_results(root, db), class = "sepsisfactorsdb_schema_mismatch")
})

test_that("invalid results are rejected before anything is written", {
  root <- local_results()
  edit_table(root, "TOY2", "fit_params", function(df) df[, c("fit_key", "name")])
  db <- local_db()
  expect_error(ingest_results(root, db), class = "sepsisfactorsdb_invalid_results")
  expect_false(file.exists(db))
})
