# A file standing in for the download: a copy of the fixture, or whatever `content` says.
fake_fetch <- function(content = NULL) {
  force(content)
  function(url, dest) {
    if (is.null(content)) {
      file.copy(testthat::test_path("fixtures", "stability.sqlite"), dest)
    } else {
      writeLines(content, dest)
    }
    invisible(dest)
  }
}

test_that("the published file has a fixed address", {
  expect_equal(
    db_url("db_no_rose.sqlite"),
    paste0(
      "https://huggingface.co/datasets/cmatkhan/sepsis_latent_factors_db_data/resolve/main/",
      "db_no_rose.sqlite"
    )
  )
})

test_that("an existing file is kept unless overwrite is set", {
  dir <- withr::local_tempdir()
  writeLines("mine", file.path(dir, "db_no_rose.sqlite"))
  local_mocked_bindings(fetch_file = function(url, dest) stop("should not download"))
  expect_message(path <- download_db(dir), "already exists")
  expect_equal(path, file.path(dir, "db_no_rose.sqlite"))
  expect_equal(readLines(path), "mine")
})

test_that("a good download is renamed into place and leaves nothing else", {
  dir <- file.path(withr::local_tempdir(), "new", "dir")
  local_mocked_bindings(fetch_file = fake_fetch())
  withr::local_options(timeout = 60)
  path <- suppressMessages(download_db(dir))
  expect_equal(path, file.path(dir, "db_no_rose.sqlite"))
  expect_equal(list.files(dir), "db_no_rose.sqlite")
  expect_equal(db_file_problem(path), NULL)
  # the timeout is raised only while downloading
  expect_equal(getOption("timeout"), 60)
})

test_that("the timeout covers a multi-gigabyte download", {
  dir <- withr::local_tempdir()
  seen <- withr::local_tempfile()
  local_mocked_bindings(
    fetch_file = function(url, dest) {
      writeLines(as.character(getOption("timeout")), seen)
      file.copy(testthat::test_path("fixtures", "stability.sqlite"), dest)
    }
  )
  suppressMessages(download_db(dir))
  expect_gte(as.numeric(readLines(seen)), DOWNLOAD_TIMEOUT)
})

test_that("overwrite replaces the file", {
  dir <- withr::local_tempdir()
  writeLines("old", file.path(dir, "db_no_rose.sqlite"))
  local_mocked_bindings(fetch_file = fake_fetch())
  suppressMessages(path <- download_db(dir, overwrite = TRUE))
  expect_null(db_file_problem(path))
})

test_that("a file that is not SQLite is removed with an explanation", {
  dir <- withr::local_tempdir()
  local_mocked_bindings(fetch_file = fake_fetch("<html>not found</html>"))
  expect_snapshot(download_db(dir), error = TRUE, transform = function(x) {
    gsub(dir, "<dir>", x, fixed = TRUE)
  })
  expect_equal(list.files(dir), character(0))
})

test_that("a database of another schema version is removed with an explanation", {
  dir <- withr::local_tempdir()
  other <- scratch_db(3L)
  local_mocked_bindings(
    fetch_file = function(url, dest) {
      file.copy(other, dest)
      invisible(dest)
    }
  )
  expect_error(suppressMessages(download_db(dir)), "schema version 3")
  expect_equal(list.files(dir), character(0))
})

test_that("a failed download leaves no partial file", {
  dir <- withr::local_tempdir()
  local_mocked_bindings(
    fetch_file = function(url, dest) {
      writeLines("half", dest)
      cli::cli_abort("connection lost")
    }
  )
  expect_error(suppressMessages(download_db(dir)), "connection lost")
  expect_equal(list.files(dir), character(0))
})

test_that("the schema check reads the file's version", {
  expect_null(db_file_problem(testthat::test_path("fixtures", "stability.sqlite")))
  expect_match(db_file_problem(scratch_db(2L)), "version 2")
  tmp <- withr::local_tempfile()
  writeLines("text", tmp)
  expect_match(db_file_problem(tmp), "not an SQLite")
})
