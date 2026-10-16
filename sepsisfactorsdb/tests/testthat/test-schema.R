test_that("a new database gets the schema, views and version", {
  db <- local_db()
  con <- open_stability_db(db)
  withr::defer(DBI::dbDisconnect(con))
  expect_equal(db_schema_version(con), SCHEMA_VERSION)
  expect_contains(DBI::dbListTables(con), c("fits", "factors", "v_fits", "v_factor_matches"))
  expect_equal(DBI::dbGetQuery(con, "PRAGMA foreign_keys")$foreign_keys, 1)
})

test_that("an existing database of the right version reopens", {
  db <- local_db()
  DBI::dbDisconnect(open_stability_db(db))
  con <- open_stability_db(db)
  withr::defer(DBI::dbDisconnect(con))
  expect_equal(db_schema_version(con), SCHEMA_VERSION)
})

test_that("a database of another version is refused", {
  db <- local_db()
  con <- open_stability_db(db)
  DBI::dbExecute(con, "PRAGMA user_version = 2")
  DBI::dbDisconnect(con)
  expect_snapshot(open_stability_db(db), error = TRUE, transform = scrub_tmp)
  expect_error(open_stability_db(db), class = "sepsisfactorsdb_schema_mismatch")
})

test_that("read_sql_statements splits on lines holding only ;;", {
  path <- withr::local_tempfile(fileext = ".sql")
  writeLines(c("-- comment", "CREATE TABLE a (x)", ";;", "CREATE TABLE b (y)", ";;", "-- only a comment"), path)
  expect_equal(read_sql_statements(path), c("-- comment\nCREATE TABLE a (x)", "CREATE TABLE b (y)"))
})
