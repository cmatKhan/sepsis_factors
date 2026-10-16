test_that("the merge finds surrogate ids and the keys that refer to them in the schema", {
  con <- open_stability_db(local_db())
  withr::defer(DBI::dbDisconnect(con))
  s <- id_structure(con)
  expect_setequal(
    s$own,
    c("fits", "factors", "fit_pairs", "factor_matches", "enrichment_queries", "projections", "pattern_drivers", "method_agreement")
  )
  expect_equal(s$surrogate$fits, "fit_id")
  fk <- function(t, from) s$shifted$ref_table[s$shifted$table == t & s$shifted$from == from]
  expect_equal(fk("factors", "fit_id"), "fits")
  expect_equal(fk("fit_pairs", "fit_b"), "fits")
  expect_equal(fk("driver_genes", "driver_id"), "pattern_drivers")
  expect_equal(fk("enrichment_results", "query_id"), "enrichment_queries")
  # a text key is not shifted, and a table follows the tables it refers to
  expect_length(fk("fits", "dataset_id"), 0)
  expect_lt(match("datasets", s$tables), match("fits", s$tables))
  expect_lt(match("fits", s$tables), match("factors", s$tables))
})

test_that("merging the load parts of datasets gives the serial database, ids included", {
  root <- local_results()
  dir <- withr::local_tempdir()
  parts <- file.path(dir, c("TOY1.sqlite", "TOY2.sqlite"))
  serial <- file.path(dir, "serial.sqlite")
  for (i in 1:2) {
    load_dataset(root, parts[i], c("TOY1", "TOY2")[i], "copy", artifact_base = serial)
    load_dataset(root, serial, c("TOY1", "TOY2")[i], "copy")
  }
  merged <- merge_parts(parts, file.path(dir, "merged.sqlite"), merge_dir = file.path(dir, "m"))
  expect_equal(merged, file.path(dir, "merged.sqlite"))
  # fits of the second part are numbered after the first's
  ids <- db_query(merged, "SELECT dataset_id, MIN(fit_id) AS lo, MAX(fit_id) AS hi FROM fits GROUP BY dataset_id")
  expect_equal(ids$lo[2], ids$hi[1] + 1)
  expect_equal(differing_tables(serial, merged), character())
})

test_that("the merge leaves the old database alone when it fails", {
  root <- local_results()
  dir <- withr::local_tempdir()
  part <- file.path(dir, "TOY1.sqlite")
  load_dataset(root, part, "TOY1", "copy", artifact_base = file.path(dir, "final.sqlite"))
  final <- file.path(dir, "final.sqlite")
  writeLines("previous database", final)
  # a duplicate dataset is a primary key conflict
  expect_error(merge_parts(c(part, part), final, merge_dir = file.path(dir, "m")))
  expect_equal(readLines(final), "previous database")
})

test_that("parts of one unit must be adjacent, and parts must exist", {
  dir <- withr::local_tempdir()
  a <- file.path(dir, "a.sqlite")
  b <- file.path(dir, "b.sqlite")
  for (f in c(a, b)) DBI::dbDisconnect(open_stability_db(f))
  expect_snapshot(merge_parts(c(a, b, a), merge_dir = file.path(dir, "m"), units = c(1, 2, 1)), error = TRUE, transform = scrub_tmp)
  expect_snapshot(merge_parts(file.path(dir, "none.sqlite"), merge_dir = file.path(dir, "m")), error = TRUE, transform = scrub_tmp)
})

test_that("check_database refuses foreign key violations", {
  db <- local_db()
  con <- open_stability_db(db)
  DBI::dbExecute(con, "PRAGMA foreign_keys = OFF")
  DBI::dbExecute(con, "INSERT INTO fit_params VALUES (99, 'x', 1, NULL)")
  DBI::dbDisconnect(con)
  expect_snapshot(check_database(db), error = TRUE, transform = scrub_tmp)
  expect_error(check_database(db), class = "sepsisfactorsdb_invalid_database")
})

test_that("a database made by the merge has its indexes and is not in WAL mode", {
  root <- local_results()
  dir <- withr::local_tempdir()
  part <- file.path(dir, "TOY1.sqlite")
  load_dataset(root, part, "TOY1", "copy", artifact_base = file.path(dir, "f.sqlite"))
  out <- merge_parts(part, file.path(dir, "f.sqlite"), merge_dir = file.path(dir, "m"))
  idx <- db_query(out, "SELECT name FROM sqlite_master WHERE type = 'index' AND name LIKE 'idx_%'")$name
  expect_contains(idx, c("idx_fits_dataset_method_rank", "idx_genes_id", "idx_method_agreement_dataset"))
  expect_equal(db_query(out, "PRAGMA journal_mode")$journal_mode, "delete")
  expect_equal(db_query(out, "PRAGMA user_version")$user_version, SCHEMA_VERSION)
  expect_equal(db_query(out, "SELECT value FROM meta WHERE key = 'schema_version'")$value, as.character(SCHEMA_VERSION))
  expect_length(list.files(dir, pattern = "-wal$"), 0)
})
