test_that("the merge finds surrogate ids and the keys that refer to them in the schema", {
  con <- open_stability_db(local_db())
  withr::defer(DBI::dbDisconnect(con))
  s <- id_structure(con)
  expect_setequal(
    s$own,
    c(
      "fits",
      "factors",
      "fit_pairs",
      "factor_matches",
      "enrichment_queries",
      "projections",
      "pattern_drivers",
      "method_agreement"
    )
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

test_that("parts are renumbered: ids and the keys that refer to them are shifted", {
  dir <- withr::local_tempdir()
  parts <- file.path(dir, c("A.sqlite", "B.sqlite", "C.sqlite"))
  toy_part(parts[1], "A", n_fits = 2)
  toy_part(parts[2], "B", n_fits = 3)
  toy_part(parts[3], "C", n_fits = 2)
  merged <- merge_parts(parts, file.path(dir, "merged.sqlite"), merge_dir = file.path(dir, "m"))
  ids <- db_query(
    merged,
    "SELECT dataset_id, MIN(fit_id) AS lo, MAX(fit_id) AS hi FROM fits GROUP BY dataset_id"
  )
  expect_equal(ids$lo, c(1, 3, 6))
  expect_equal(ids$hi, c(2, 5, 7))
  expect_equal(db_query(merged, "SELECT factor_id FROM factors ORDER BY factor_id")$factor_id, 1:14)
  # a factor still belongs to the fit it was made for
  f <- db_query(
    merged,
    "SELECT x.factor_id, f.dataset_id, f.fit_key FROM factors x JOIN fits f USING (fit_id)
     ORDER BY x.factor_id"
  )
  expect_equal(f$dataset_id, rep(c("A", "B", "C"), c(4, 6, 4)))
  expect_equal(f$fit_key[7:8], c("B_fit2", "B_fit2"))
  # a pair refers to the shifted fits of its own part
  p <- db_query(merged, "SELECT fit_pair_id, fit_a, fit_b FROM fit_pairs ORDER BY fit_pair_id")
  expect_equal(p$fit_pair_id, 1:3)
  expect_equal(p$fit_a, c(1, 3, 6))
  expect_equal(p$fit_b, c(2, 4, 7))
  # methods are shared, not duplicated
  expect_equal(db_count(merged, "methods"), 1)
  expect_equal(differing_tables(merged, merged), character())
  expect_no_error(check_database(merged))
})

test_that("the chunk parts of a unit share its ids and add agreement rows after one another", {
  dir <- withr::local_tempdir()
  parts <- file.path(dir, c("A.sqlite", "B.sqlite", "B1.sqlite", "B2.sqlite"))
  toy_part(parts[1], "A", n_fits = 2, agreement = TRUE)
  # the load part of B has the fits, its chunk parts only the agreement rows that refer to them
  toy_part(parts[2], "B", n_fits = 2)
  for (k in 3:4) {
    toy_part(parts[k], "B", n_fits = 2, agreement = TRUE)
    con <- open_stability_db(parts[k])
    DBI::dbExecute(con, "PRAGMA foreign_keys = OFF")
    for (t in c("datasets", "fits", "factors", "fit_pairs")) {
      DBI::dbExecute(con, paste("DELETE FROM", t))
    }
    DBI::dbExecute(con, sprintf("UPDATE method_agreement SET top_n = %d", 10L * (k - 2L)))
    DBI::dbDisconnect(con)
  }
  merged <- merge_parts(
    parts, file.path(dir, "merged.sqlite"),
    merge_dir = file.path(dir, "m"), units = c(1, 2, 2, 2)
  )
  expect_equal(db_count(merged, "fits"), 4)
  a <- db_query(merged, "SELECT agreement_id, dataset_id, fit_a, fit_b FROM method_agreement")
  expect_equal(a$agreement_id, 1:3)
  expect_equal(a$dataset_id, c("A", "B", "B"))
  # both chunks of B refer to B's fits 3 and 4, the numbers B's load part got
  expect_equal(a$fit_a, c(1, 3, 3))
  expect_equal(a$fit_b, c(2, 4, 4))
  expect_no_error(check_database(merged))
})

test_that("parts whose ids are final are added without renumbering", {
  dir <- withr::local_tempdir()
  a <- file.path(dir, "A.sqlite")
  b <- file.path(dir, "B.sqlite")
  toy_part(a, "A", n_fits = 2, agreement = TRUE)
  toy_part(b, "A", n_fits = 2, agreement = TRUE)
  con <- open_stability_db(b)
  DBI::dbExecute(con, "PRAGMA foreign_keys = OFF")
  for (t in c("datasets", "fits", "factors", "fit_pairs")) {
    DBI::dbExecute(con, paste("DELETE FROM", t))
  }
  DBI::dbExecute(con, "UPDATE method_agreement SET agreement_id = 5, top_n = 25")
  DBI::dbDisconnect(con)
  into <- merge_parts(a, merge_dir = file.path(dir, "m"))
  merge_parts(b, into = into, renumber = FALSE, units = 1)
  expect_equal(
    db_query(into, "SELECT agreement_id FROM method_agreement ORDER BY 1")$agreement_id,
    c(1, 5)
  )
})

test_that("the merge leaves the old database alone when it fails", {
  dir <- withr::local_tempdir()
  part <- file.path(dir, "TOY1.sqlite")
  toy_part(part, "A")
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
  expect_snapshot(
    merge_parts(c(a, b, a), merge_dir = file.path(dir, "m"), units = c(1, 2, 1)),
    error = TRUE,
    transform = scrub_tmp
  )
  expect_snapshot(
    merge_parts(file.path(dir, "none.sqlite"), merge_dir = file.path(dir, "m")),
    error = TRUE,
    transform = scrub_tmp
  )
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
  dir <- withr::local_tempdir()
  part <- file.path(dir, "A.sqlite")
  toy_part(part, "A")
  out <- merge_parts(part, file.path(dir, "f.sqlite"), merge_dir = file.path(dir, "m"))
  idx <- db_query(
    out,
    "SELECT name FROM sqlite_master WHERE type = 'index' AND name LIKE 'idx_%'"
  )$name
  expect_contains(
    idx,
    c("idx_fits_dataset_method_rank", "idx_genes_id", "idx_method_agreement_dataset")
  )
  expect_equal(db_query(out, "PRAGMA journal_mode")$journal_mode, "delete")
  expect_equal(db_query(out, "PRAGMA user_version")$user_version, SCHEMA_VERSION)
  expect_equal(
    db_query(out, "SELECT value FROM meta WHERE key = 'schema_version'")$value,
    as.character(SCHEMA_VERSION)
  )
  expect_length(list.files(dir, pattern = "-wal$"), 0)
})
