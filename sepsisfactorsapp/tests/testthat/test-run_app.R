test_that("app() needs a database", {
  expect_error(app(db_path = ""), "No database given")
  expect_error(app(db_path = file.path(tempdir(), "none.sqlite")), "No stability database")
})

test_that("app() refuses a database of another schema version", {
  expect_error(app(scratch_db(2L)), "schema version 2")
  expect_error(app(scratch_db(4L)), "reads version 3")
})

test_that("app() refuses a config directory that does not exist", {
  expect_error(
    app(scratch_db(3L), config_dir = file.path(tempdir(), "no_such_dir")),
    "does not exist"
  )
})

test_that("config files come from the configured directory only", {
  dir <- withr::local_tempdir()
  writeLines(c("A: [A_D1, A_D2, A_D10]"), file.path(dir, "dataset_families.yml"))
  withr::local_options(sepsisfactorsapp.config_dir = NULL)
  expect_true(is.na(config_file("dataset_families.yml")))
  withr::local_options(sepsisfactorsapp.config_dir = dir)
  expect_equal(basename(config_file("dataset_families.yml")), "dataset_families.yml")
  expect_true(is.na(config_file("composition_markers.yml")))
})

test_that("timepoints follow the family file, else the number in the id", {
  dir <- withr::local_tempdir()
  writeLines("A: [A_PRE, A_D2, A_D1]", file.path(dir, "dataset_families.yml"))
  withr::local_options(sepsisfactorsapp.config_dir = dir)
  expect_equal(order_timepoints(c("A_D1", "A_PRE", "A_D2")), c(2, 3, 1))
  withr::local_options(sepsisfactorsapp.config_dir = NULL)
  expect_equal(order_timepoints(c("A_D10", "A_D2", "A_PRE")), c(3, 2, 1))
})
