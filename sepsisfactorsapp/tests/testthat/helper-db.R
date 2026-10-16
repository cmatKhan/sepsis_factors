# The tiny schema-4 fixture (built by dev/make_fixture.R from sepsisfactorsdb's
# example results: datasets TOY1 and TOY2, all six methods on TOY1).
fixture_path <- function() {
  testthat::test_path("fixtures", "stability.sqlite")
}

# The fixture opened read-only, closed when `env` (a file's teardown by default) ends.
# Open it once per test file at the top level and share it between that file's tests.
open_fixture <- function(env = testthat::teardown_env()) {
  db <- open_db(fixture_path())
  withr::defer(close_db(db), envir = env)
  db
}

# A writable copy of the fixture in a scratch directory, opened read-only as the app
# does. Its directory is where full artifacts would sit. Use it when a test needs
# artifact files or has to change the database.
fixture_copy <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  file.copy(fixture_path(), file.path(dir, "stability.sqlite"))
  db <- open_db(file.path(dir, "stability.sqlite"))
  withr::defer(close_db(db), envir = env)
  db
}

# Write an artifact where the database's pointer says it is.
write_artifact <- function(db, path, object) {
  file <- file.path(db$dir, path)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(object, file)
  invisible(file)
}

# A scratch SQLite file with the given `PRAGMA user_version`.
scratch_db <- function(version, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".sqlite", .local_envir = env)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, sprintf("PRAGMA user_version = %d", version))
  DBI::dbDisconnect(con)
  path
}

# The writer's encoders, to build blobs for the decoders.
encode_f32 <- function(x) {
  memCompress(writeBin(as.numeric(x), raw(), size = 4L, endian = "little"), "gzip")
}
encode_i32 <- function(x) {
  memCompress(writeBin(as.integer(x), raw(), size = 4L, endian = "little"), "gzip")
}
encode_ids <- function(x) {
  as.character(jsonlite::toJSON(as.character(x)))
}

# The real database tests are opt-in: set STABILITY_DB to a schema-4 database. They read
# it where it is and never copy or modify it.
local_real_db <- function(env = parent.frame()) {
  testthat::skip_if(!nzchar(Sys.getenv("STABILITY_DB")), "STABILITY_DB is not set")
  testthat::skip_if_not(file.exists(Sys.getenv("STABILITY_DB")), "STABILITY_DB does not exist")
  db <- open_db(Sys.getenv("STABILITY_DB"))
  withr::defer(close_db(db), envir = env)
  db
}
