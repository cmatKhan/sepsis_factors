db <- open_fixture()

test_that("the database opens read-only at the schema version of the fixture", {
  expect_s3_class(db, "stability_db")
  expect_equal(db_schema_version(db), EXPECTED_SCHEMA_VERSION)
  expect_error(DBI::dbExecute(db$con, "DELETE FROM datasets"))
})

test_that("queries take bound parameters, never pasted values", {
  d <- db_query(db, "SELECT dataset_id FROM datasets WHERE dataset_id = ?", list("TOY1"))
  expect_equal(d$dataset_id, "TOY1")
  d <- db_query(
    db, "SELECT dataset_id FROM datasets WHERE dataset_id = ?", list("x' OR '1'='1")
  )
  expect_equal(nrow(d), 0)
})

test_that("a missing table is a classed condition that if_missing() turns into a notice", {
  expect_error(
    db_query(db, "SELECT * FROM no_such_table"),
    class = "sepsisfactorsapp_missing_table"
  )
  expect_null(suppressMessages(if_missing(db_query(db, "SELECT * FROM no_such_table"), "x")))
  expect_equal(if_missing(1 + 1, "x"), 2)
  expect_error(db_query(db, "SELEKT 1"), class = "error")
  expect_equal(db_missing_tables(db, c("datasets", "nope")), "nope")
})
