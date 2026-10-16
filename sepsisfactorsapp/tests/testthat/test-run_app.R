test_that("app() needs a database", {
  expect_error(app(db_path = ""), "No database given")
  expect_error(app(db_path = file.path(tempdir(), "none.sqlite")), "No stability database")
})

test_that("app() refuses a database of another schema version", {
  expect_error(app(scratch_db(3L)), "schema version 3")
  expect_error(app(scratch_db(5L)), "reads version 4")
})

test_that("the expected schema version is the one sepsisfactorsdb writes", {
  schema <- test_path("..", "..", "..", "sepsisfactorsdb", "R", "schema.R")
  skip_if_not(file.exists(schema), "the sibling sepsisfactorsdb directory is not here")
  line <- grep("^SCHEMA_VERSION <- [0-9]+L", readLines(schema), value = TRUE)
  expect_length(line, 1)
  version <- as.integer(sub("^SCHEMA_VERSION <- ([0-9]+)L.*", "\\1", line))
  expect_equal(version, EXPECTED_SCHEMA_VERSION)
})

test_that("the fixture is at the schema version the app reads", {
  expect_null(db_file_problem(fixture_path()))
})
