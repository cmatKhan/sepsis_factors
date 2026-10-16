test_that("a legacy tree converts to a results output that loads", {
  leg <- local_legacy()
  out <- file.path(withr::local_tempdir(), "res")
  legacy_to_results(leg$root, out, leg$config, artifact_mode = "symlink")
  manifest <- validate_results(out)
  expect_equal(manifest$results_output_version, RESULTS_OUTPUT_VERSION)
  expect_equal(manifest$datasets, c("LA", "LB"))
  expect_equal(names(manifest$methods), "pca")

  db <- local_db()
  ingest_results(out, db, artifact_mode = "symlink")
  d <- db_query(db, "SELECT dataset_id, family, family_position, hf_collection FROM datasets ORDER BY dataset_id")
  expect_equal(d$family, c("FAM", "FAM"))
  expect_equal(d$family_position, c(2, 1))
  expect_equal(d$hf_collection, c("org/coll", NA))
  expect_equal(db_query(db, "SELECT marker_set, gene FROM composition_markers ORDER BY marker_set, gene")$gene, c("CCC", "AAA", "BBB"))
  expect_equal(db_count(db, "projections"), 1)
  expect_equal(db_count(db, "projection_values"), 1)
  expect_equal(db_count(db, "factor_top_genes") > 0, TRUE)
  expect_equal(db_count(db, "time_gene_recurrence"), 1)
  expect_equal(db_query(db, "SELECT symbol FROM genes WHERE dataset_id = 'LA' AND gene_idx = 2")$symbol, "G2")
})

test_that("only the artifacts the database or the derive step needs are linked", {
  leg <- local_legacy()
  out <- file.path(withr::local_tempdir(), "res")
  legacy_to_results(leg$root, out, leg$config, artifact_mode = "symlink")
  files <- list.files(file.path(out, "datasets", "LA", "artifacts"))
  expect_false(any(grepl("_raw\\.rds$|^matrix", files)))
  expect_setequal(
    sub("^pca_[0-9a-f]+_", "", files[grepl("^pca_", files)]),
    c("loadings.rds", "scores.rds", "diag.rds")
  )
  expect_contains(files, c("sample_metadata.rds", "feature_metadata.rds"))
})

test_that("symlink mode links to the source file in place and never copies", {
  leg <- local_legacy()
  out <- file.path(withr::local_tempdir(), "res")
  legacy_to_results(leg$root, out, leg$config, artifact_mode = "symlink")
  link <- file.path(out, "datasets", "LA", "artifacts", "sample_metadata.rds")
  expect_equal(unname(Sys.readlink(link)), normalizePath(file.path(leg$root, "stability_artifacts", "LA", "sample_metadata.rds")))
  # through the loader's own symlink mode the database tree points at the source too
  db <- local_db()
  ingest_results(out, db, artifact_mode = "symlink")
  placed <- list.files(artifacts_dir(db, "LA"), full.names = TRUE)
  expect_true(all(nzchar(Sys.readlink(placed))))
  expect_true(all(startsWith(normalizePath(placed), normalizePath(leg$root))))
})

test_that("hard-link mode links, and auto picks a hard link on one filesystem", {
  leg <- local_legacy()
  for (mode in c("link", "auto")) {
    out <- file.path(withr::local_tempdir(), "res")
    suppressWarnings(legacy_to_results(leg$root, out, leg$config, artifact_mode = mode))
    link <- file.path(out, "datasets", "LA", "artifacts", "sample_metadata.rds")
    expect_true(file.exists(link))
    expect_equal(unname(Sys.readlink(link)), "")
  }
})

test_that("copy mode copies and a file that cannot be placed is an error", {
  leg <- local_legacy()
  out <- file.path(withr::local_tempdir(), "res")
  legacy_to_results(leg$root, out, leg$config, artifact_mode = "copy")
  expect_equal(unname(Sys.readlink(file.path(out, "datasets", "LA", "artifacts", "sample_metadata.rds"))), "")
  dir <- withr::local_tempdir()
  writeLines("x", file.path(dir, "blocker"))
  src <- file.path(dir, "src")
  writeLines("y", src)
  expect_snapshot(legacy_place(src, file.path(dir, "blocker", "x"), "symlink"), error = TRUE, transform = scrub_tmp)
})

test_that("projections whose target is not converted are left out", {
  leg <- local_legacy()
  out <- file.path(withr::local_tempdir(), "res")
  legacy_to_results(leg$root, out, leg$config, datasets = "LA", artifact_mode = "symlink")
  expect_false(file.exists(file.path(out, "cross_dataset", "tables", "projections.parquet")))
  db <- local_db()
  ingest_results(out, db, artifact_mode = "symlink")
  expect_equal(db_count(db, "datasets"), 1)
  expect_equal(db_count(db, "projections"), 0)
})

test_that("a read-only legacy database opens (immutable)", {
  leg <- local_legacy()
  Sys.chmod(file.path(leg$root, "stability.sqlite"), "444")
  Sys.chmod(leg$root, "555")
  withr::defer(Sys.chmod(leg$root, "755"))
  out <- file.path(withr::local_tempdir(), "res")
  expect_no_error(legacy_to_results(leg$root, out, leg$config, artifact_mode = "symlink"))
})

test_that("a non-empty output, another schema version and an unknown dataset are refused", {
  leg <- local_legacy()
  out <- withr::local_tempdir()
  writeLines("x", file.path(out, "f"))
  expect_snapshot(legacy_to_results(leg$root, out, leg$config), error = TRUE, transform = scrub_tmp)
  expect_snapshot(
    legacy_to_results(leg$root, file.path(withr::local_tempdir(), "r"), leg$config, datasets = "NOPE"),
    error = TRUE,
    transform = scrub_tmp
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(leg$root, "stability.sqlite"))
  DBI::dbExecute(con, "PRAGMA user_version = 2")
  DBI::dbDisconnect(con)
  expect_snapshot(legacy_to_results(leg$root, file.path(withr::local_tempdir(), "r"), leg$config), error = TRUE, transform = scrub_tmp)
})
