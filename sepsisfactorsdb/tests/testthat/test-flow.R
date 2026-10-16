test_that("use_ingest(serial = TRUE) writes a _targets.R once", {
  dir <- withr::local_tempdir()
  path <- suppressMessages(use_ingest(dir, serial = TRUE))
  expect_true(file.exists(path))
  expect_match(
    readLines(path)[grep("ingest_targets", readLines(path))],
    "sepsisfactorsdb::ingest_targets"
  )
  expect_error(suppressMessages(use_ingest(dir, serial = TRUE)), "already exist")
})
