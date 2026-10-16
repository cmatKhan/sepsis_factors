#' Schema version
#'
#' The integer stamped into `PRAGMA user_version`. It changes only when
#' `schema.sql` or `views.sql` change, and it alone decides whether a database
#' can be opened: [open_stability_db()] refuses any other version. There are
#' no migrations.
#'
#' @format An integer.
#' @export
SCHEMA_VERSION <- 4L

#' Results-output version
#'
#' The version of the results-output format (see [validate_results()]) that
#' this package loads. It is read from `manifest.json`. Version 2 adds only
#' optional parts to version 1 (dataset family columns, `composition_markers`).
#'
#' @format An integer.
#' @export
RESULTS_OUTPUT_VERSION <- 2L

#' Path to one of the package's SQL files
#'
#' @param file `"schema.sql"` or `"views.sql"`.
#' @return The installed path.
#' @noRd
sql_path <- function(file) {
  path <- system.file("sql", file, package = "sepsisfactorsdb")
  if (!nzchar(path)) {
    cli::cli_abort("The package's {.file {file}} is missing; reinstall {.pkg sepsisfactorsdb}.")
  }
  path
}

#' Split a schema file into statements
#'
#' Statements are separated by lines holding only `;;`, since a statement may
#' itself contain semicolons.
#'
#' @param path Path to a `.sql` file.
#' @return Character vector of non-empty statements.
#' @noRd
read_sql_statements <- function(path) {
  sql <- paste(readLines(path), collapse = "\n")
  parts <- trimws(strsplit(sql, "\n;;\n", fixed = TRUE)[[1]])
  parts[nzchar(trimws(gsub("--[^\n]*", "", parts)))]
}

#' Open the stability database for writing
#'
#' Creates the file from `schema.sql` and `views.sql` if it is new, stamps
#' `PRAGMA user_version` and fills the `meta` table; otherwise refuses a database whose version is not
#' [SCHEMA_VERSION]. Sets `foreign_keys = ON`, WAL journaling,
#' `synchronous = NORMAL` and a 30 s busy timeout.
#'
#' @param db_path Path to the SQLite file.
#' @return A DBI connection; close it with [DBI::dbDisconnect()].
#' @export
open_stability_db <- function(db_path) {
  open_db(db_path, indexes = TRUE)
}

#' Open or create the database, optionally without secondary indexes
#'
#' The merge of a parallel build creates the final database without the
#' secondary indexes and adds them after the rows are in
#' (`create_indexes()`), which is faster than maintaining them per insert.
#'
#' @param db_path Path to the SQLite file.
#' @param indexes Create the secondary indexes of a new database.
#' @noRd
open_db <- function(db_path, indexes) {
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  is_new <- !file.exists(db_path) || file.size(db_path) == 0
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  # WAL + synchronous = NORMAL: no full fsync on every commit (costly on
  # network filesystems). If db_path's filesystem cannot do WAL's shared
  # memory locking, SQLite fails loudly here.
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
  DBI::dbExecute(con, "PRAGMA synchronous = NORMAL")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 30000")

  if (is_new) {
    DBI::dbExecute(con, "BEGIN")
    for (stmt in schema_statements(indexes = indexes)) DBI::dbExecute(con, stmt)
    DBI::dbExecute(con, sprintf("PRAGMA user_version = %d", SCHEMA_VERSION))
    DBI::dbAppendTable(
      con,
      "meta",
      data.frame(
        key = c("schema_version", "created_at", "writer", "writer_version"),
        value = c(
          as.character(SCHEMA_VERSION),
          utc_now(),
          "sepsisfactorsdb",
          as.character(utils::packageVersion("sepsisfactorsdb"))
        )
      )
    )
    DBI::dbExecute(con, "COMMIT")
  } else {
    version <- db_schema_version(con)
    if (!identical(version, SCHEMA_VERSION)) {
      DBI::dbDisconnect(con)
      cli::cli_abort(
        c(
          "{.file {db_path}} has schema version {version}, but this package writes version {SCHEMA_VERSION}.",
          "i" = "Delete the {.file .sqlite} file (keep {.file stability_artifacts/}) and load again; there are no migrations."
        ),
        class = "sepsisfactorsdb_schema_mismatch",
        call = sys.call(-1)
      )
    }
  }
  con
}

#' The statements that create the schema and the views
#'
#' @param indexes Include the `CREATE INDEX` statements.
#' @return Character vector, tables first, then views, then indexes if asked.
#' @noRd
schema_statements <- function(indexes = TRUE) {
  stmts <- c(read_sql_statements(sql_path("schema.sql")), read_sql_statements(sql_path("views.sql")))
  is_index <- grepl("^\\s*(--[^\n]*\n\\s*)*CREATE (UNIQUE )?INDEX", stmts)
  c(stmts[!is_index], if (indexes) stmts[is_index])
}

#' Create the secondary indexes a database was made without
#'
#' @param con A connection to a database made with `open_db(indexes = FALSE)`.
#' @noRd
create_indexes <- function(con) {
  stmts <- c(read_sql_statements(sql_path("schema.sql")), read_sql_statements(sql_path("views.sql")))
  for (stmt in stmts[grepl("^\\s*(--[^\n]*\n\\s*)*CREATE (UNIQUE )?INDEX", stmts)]) DBI::dbExecute(con, stmt)
  invisible(NULL)
}

#' A database's schema version
#'
#' @param con A DBI connection to a stability database.
#' @return The `PRAGMA user_version`, as an integer.
#' @export
db_schema_version <- function(con) {
  as.integer(DBI::dbGetQuery(con, "PRAGMA user_version")$user_version)
}
