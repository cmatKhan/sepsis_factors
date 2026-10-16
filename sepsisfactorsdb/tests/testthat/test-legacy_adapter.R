test_that("a legacy tree converts to a results output that loads, with symlinked artifacts", {
  leg <- local_legacy()
  out <- file.path(withr::local_tempdir(), "res")
  legacy_to_results(leg$root, out, leg$config, artifact_mode = "symlink")
  manifest <- validate_results(out)
  expect_equal(manifest$results_output_version, RESULTS_OUTPUT_VERSION)
  expect_equal(manifest$datasets, c("LA", "LB"))
  expect_equal(names(manifest$methods), "pca")
  files <- list.files(file.path(out, "datasets", "LA", "artifacts"))
  expect_false(any(grepl("_raw\\.rds$|^matrix", files)))
  expect_setequal(
    sub("^pca_[0-9a-f]+_", "", files[grepl("^pca_", files)]),
    c("loadings.rds", "scores.rds", "diag.rds")
  )
  expect_contains(files, c("sample_metadata.rds", "feature_metadata.rds"))
  link <- file.path(out, "datasets", "LA", "artifacts", "sample_metadata.rds")
  expect_equal(
    unname(Sys.readlink(link)),
    normalizePath(file.path(leg$root, "stability_artifacts", "LA", "sample_metadata.rds"))
  )

  db <- local_db()
  ingest_results(out, db, artifact_mode = "symlink")
  d <- db_query(
    db,
    "SELECT dataset_id, family, family_position, hf_collection FROM datasets ORDER BY dataset_id"
  )
  expect_equal(d$family, c("FAM", "FAM"))
  expect_equal(d$family_position, c(2, 1))
  expect_equal(d$hf_collection, c("org/coll", NA))
  expect_equal(
    db_query(db, "SELECT marker_set, gene FROM composition_markers ORDER BY marker_set, gene")$gene,
    c("CCC", "AAA", "BBB")
  )
  expect_equal(db_count(db, "projections"), 1)
  expect_equal(db_count(db, "projection_values"), 1)
  expect_equal(db_count(db, "factor_top_genes") > 0, TRUE)
  expect_equal(db_count(db, "time_gene_recurrence"), 1)
  expect_equal(
    db_query(db, "SELECT symbol FROM genes WHERE dataset_id = 'LA' AND gene_idx = 2")$symbol,
    "G2"
  )
})

test_that("auto mode picks a hard link on one filesystem", {
  dir <- withr::local_tempdir()
  art <- file.path(dir, "art")
  dir.create(art)
  writeLines("x", file.path(art, "a"))
  expect_equal(resolve_legacy_mode("auto", art, file.path(dir, "out")), "link")
  expect_equal(resolve_legacy_mode("copy", art, file.path(dir, "out")), "copy")
  empty <- file.path(dir, "empty")
  expect_equal(resolve_legacy_mode("auto", empty, file.path(dir, "out")), "symlink")
})

test_that("copy mode copies and a file that cannot be placed is an error", {
  dir <- withr::local_tempdir()
  src <- file.path(dir, "src")
  writeLines("y", src)
  legacy_place(src, file.path(dir, "hard"), "link")
  expect_equal(unname(Sys.readlink(file.path(dir, "hard"))), "")
  expect_equal(readLines(file.path(dir, "hard")), "y")
  legacy_place(src, file.path(dir, "sub", "copy"), "copy")
  expect_equal(unname(Sys.readlink(file.path(dir, "sub", "copy"))), "")
  expect_equal(readLines(file.path(dir, "sub", "copy")), "y")
  legacy_place(src, file.path(dir, "sym"), "symlink")
  expect_equal(unname(Sys.readlink(file.path(dir, "sym"))), normalizePath(src))
  expect_error(legacy_place(file.path(dir, "none"), file.path(dir, "x"), "copy"), "missing")
  writeLines("x", file.path(dir, "blocker"))
  expect_snapshot(
    legacy_place(src, file.path(dir, "blocker", "x"), "symlink"),
    error = TRUE,
    transform = scrub_tmp
  )
})

test_that("a non-empty output, another schema version and an unknown dataset are refused", {
  leg <- local_legacy()
  out <- withr::local_tempdir()
  writeLines("x", file.path(out, "f"))
  expect_snapshot(legacy_to_results(leg$root, out, leg$config), error = TRUE, transform = scrub_tmp)
  expect_snapshot(
    legacy_to_results(
      leg$root,
      file.path(withr::local_tempdir(), "r"),
      leg$config,
      datasets = "NOPE"
    ),
    error = TRUE,
    transform = scrub_tmp
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(leg$root, "stability.sqlite"))
  DBI::dbExecute(con, "PRAGMA user_version = 2")
  DBI::dbDisconnect(con)
  expect_snapshot(
    legacy_to_results(leg$root, file.path(withr::local_tempdir(), "r"), leg$config),
    error = TRUE,
    transform = scrub_tmp
  )
})
