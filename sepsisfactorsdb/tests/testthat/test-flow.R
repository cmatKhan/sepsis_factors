test_that("use_ingest writes a _targets.R once", {
  dir <- withr::local_tempdir()
  path <- suppressMessages(use_ingest(dir))
  expect_true(file.exists(path))
  expect_match(readLines(path)[grep("ingest_targets", readLines(path))], "sepsisfactorsdb::ingest_targets")
  expect_error(suppressMessages(use_ingest(dir)), "already exists")
})

test_that("the workflow loads datasets, and a rerun skips what did not change", {
  root <- local_results()
  db <- local_db()
  proj <- withr::local_tempdir()
  script <- file.path(proj, "_targets.R")
  store <- file.path(proj, "store")
  writeLines(
    c(
      "library(targets)",
      "targets::tar_option_set(packages = character(), error = 'stop')",
      sprintf("sepsisfactorsdb::ingest_targets(%s, %s)", deparse(root), deparse(db))
    ),
    script
  )
  make <- function() {
    targets::tar_make(script = script, store = store, callr_function = NULL, reporter = "silent")
  }
  outdated <- function() {
    targets::tar_outdated(script = script, store = store, callr_function = NULL)
  }

  expect_contains(outdated(), c("db_TOY1", "db_TOY2", "db_cross_dataset"))
  make()
  expect_equal(db_count(db, "datasets"), 2)
  expect_equal(db_count(db, "projections"), 1)
  expect_length(outdated(), 0)

  edit_table(root, "TOY2", "fit_params", function(df) df[-1, ])
  expect_setequal(outdated(), c("results_TOY2", "db_TOY2", "db_cross_dataset"))
  make()
  expect_length(outdated(), 0)
})
