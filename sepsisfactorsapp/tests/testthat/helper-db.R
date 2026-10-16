# The real-database tests are opt-in: set STABILITY_DB to a schema-3 database
# (artifacts beside it). They never copy or modify it.
skip_if_no_db <- function() {
  skip_if(!nzchar(Sys.getenv("STABILITY_DB")), "STABILITY_DB is not set")
  skip_if_not(file.exists(Sys.getenv("STABILITY_DB")), "STABILITY_DB does not exist")
}

# A read-only connection with the artifact root set, closed at the end of the test.
local_real_db <- function(env = parent.frame()) {
  skip_if_no_db()
  path <- Sys.getenv("STABILITY_DB")
  con <- connect_read_only(path)
  withr::defer(DBI::dbDisconnect(con), envir = env)
  withr::local_options(stability.db_dir = normalizePath(dirname(path)), .local_envir = env)
  con
}

# A scratch SQLite file with the given `PRAGMA user_version`.
scratch_db <- function(version, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".sqlite", .local_envir = env)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, sprintf("PRAGMA user_version = %d", version))
  DBI::dbDisconnect(con)
  path
}
