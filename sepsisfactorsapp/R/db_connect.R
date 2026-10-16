# The data-access layer: db_*.R are the only files that write SQL. A database is a
# handle (list with the connection, the file and its directory) passed to every
# function; queries use bound parameters.

# Open the stability database read-only and return its handle. A database in WAL mode
# on a read-only file system (e.g. a mounted snapshot) cannot be opened normally,
# because SQLite wants to create its -shm file; it is then opened as an immutable
# snapshot, which means the file must not change while the app runs.
open_db <- function(path) {
  path <- normalizePath(path)
  con <- connect_read_only(path)
  structure(list(con = con, path = path, dir = dirname(path)), class = "stability_db")
}

close_db <- function(db) {
  DBI::dbDisconnect(db$con)
  invisible(NULL)
}

connect_read_only <- function(path) {
  con <- tryCatch(
    DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO, synchronous = NULL),
    error = function(e) NULL
  )
  works <- !is.null(con) && tryCatch(
    {
      DBI::dbGetQuery(con, "SELECT 1 FROM sqlite_master LIMIT 1")
      TRUE
    },
    error = function(e) FALSE
  )
  if (works) {
    return(con)
  }
  if (!is.null(con)) {
    try(DBI::dbDisconnect(con), silent = TRUE)
  }
  cli::cli_inform(c(
    "i" = "Could not open {.file {path}} read-only in place; opening it as an immutable snapshot.",
    " " = "Restart the app after replacing the file."
  ))
  DBI::dbConnect(
    RSQLite::SQLite(), paste0("file:", path, "?immutable=1"),
    flags = RSQLite::SQLITE_RO, synchronous = NULL
  )
}

# The database's schema version (`PRAGMA user_version`).
db_schema_version <- function(db) {
  as.integer(DBI::dbGetQuery(db$con, "PRAGMA user_version")[[1]])
}

# Run a query with bound parameters. A missing table or column is a classed condition
# (sepsisfactorsapp_missing_table), so a caller can turn it into a notice.
db_query <- function(db, sql, params = NULL) {
  tryCatch(
    DBI::dbGetQuery(db$con, sql, params = params),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("no such (table|column)", msg)) {
        cli::cli_abort(
          c("The database lacks something this query reads.", "x" = "{msg}"),
          class = "sepsisfactorsapp_missing_table"
        )
      }
      stop(e)
    }
  )
}

# Evaluate `expr`; a missing table is logged and gives NULL.
if_missing <- function(expr, what) {
  tryCatch(
    expr,
    sepsisfactorsapp_missing_table = function(e) {
      note_missing(what, conditionMessage(e))
      NULL
    }
  )
}

# The names among `tables` that the database does not have.
db_missing_tables <- function(db, tables) {
  have <- DBI::dbGetQuery(db$con, "SELECT name FROM sqlite_master WHERE type = 'table'")$name
  setdiff(tables, have)
}
