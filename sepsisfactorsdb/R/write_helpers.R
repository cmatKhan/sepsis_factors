#' Current time as an ISO-8601 UTC string
#'
#' @return e.g. `"2026-10-05T16:13:15Z"`.
#' @noRd
utc_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

#' Run code in a transaction
#'
#' @param con DB connection.
#' @param code Code to run; rolled back if it errors.
#' @return `NULL`, invisibly.
#' @noRd
with_transaction <- function(con, code) {
  DBI::dbExecute(con, "BEGIN")
  ok <- FALSE
  on.exit(if (!ok) DBI::dbExecute(con, "ROLLBACK"), add = TRUE)
  force(code)
  DBI::dbExecute(con, "COMMIT")
  ok <- TRUE
  invisible(NULL)
}

#' Append rows to a table
#'
#' @param con DB connection.
#' @param table Table name.
#' @param df Rows; `NULL` or empty is skipped.
#' @return Number of rows appended, invisibly.
#' @noRd
append_rows <- function(con, table, df) {
  if (is.null(df) || nrow(df) == 0) {
    return(invisible(0L))
  }
  DBI::dbAppendTable(con, table, df)
  invisible(nrow(df))
}

#' Map keys to ids, failing on unmapped keys
#'
#' @param keys Keys (a fit key, or `"<fit_key> <factor_index>"`).
#' @param ids Named integer vector (names = keys).
#' @param what Label for the error message.
#' @return Integer ids; errors naming the keys that have none.
#' @noRd
map_ids <- function(keys, ids, what) {
  out <- unname(ids[keys])
  bad <- unique(keys[is.na(out)])
  if (length(bad)) {
    cli::cli_abort(
      paste0(
        "{length(bad)} {what} {cli::qty(length(bad))}key{?s} ha{?s/ve} no id in the ",
        "database: {.val {utils::head(bad, 5)}}"
      ),
      class = "sepsisfactorsdb_unmapped_key"
    )
  }
  out
}

#' The next unused id of a table
#'
#' @param con DB connection.
#' @param table Table name.
#' @param col Id column.
#' @return `MAX(col) + 1`.
#' @noRd
next_id <- function(con, table, col) {
  sql <- sprintf("SELECT COALESCE(MAX(%s), 0) AS m FROM %s", col, table)
  as.integer(DBI::dbGetQuery(con, sql)$m) + 1L
}

#' A named id lookup for a table's natural key
#'
#' @param df Data frame with the key columns and the id column.
#' @param id_col Name of the id column.
#' @param key_cols Names of the key columns, pasted with a space.
#' @return Named integer vector.
#' @noRd
id_lookup <- function(df, id_col, key_cols) {
  keys <- do.call(paste, unname(as.list(df[key_cols])))
  stats::setNames(as.integer(df[[id_col]]), keys)
}
