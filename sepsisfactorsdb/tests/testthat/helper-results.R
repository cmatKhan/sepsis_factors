# A fresh example results output (and a path for a scratch database) that is
# removed when the calling test finishes.
local_results <- function(env = parent.frame()) {
  path <- withr::local_tempdir("results-", .local_envir = env)
  example_results(file.path(path, "res"))
}

local_db <- function(env = parent.frame()) {
  file.path(withr::local_tempdir("db-", .local_envir = env), "stability.sqlite")
}

scrub_tmp <- function(x) gsub("/[^ ']*(Rtmp|file)[A-Za-z0-9]+[^ ']*", "<tmp>", x)

db_count <- function(db, table) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, paste("SELECT COUNT(*) AS n FROM", table))$n
}

db_query <- function(db, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql)
}

# Rewrite one results table in place.
edit_table <- function(root, dataset, table, f) {
  path <- file.path(root, "datasets", dataset, "tables", paste0(table, ".parquet"))
  df <- as.data.frame(arrow::read_parquet(path))
  arrow::write_parquet(f(df), path)
}

edit_manifest <- function(root, f) {
  path <- file.path(root, "manifest.json")
  manifest <- jsonlite::read_json(path, simplifyVector = TRUE)
  jsonlite::write_json(f(manifest), path, auto_unbox = TRUE, pretty = TRUE)
}
