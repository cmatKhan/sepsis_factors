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

# Ingest a fresh example results output into a scratch database.
local_ingested <- function(env = parent.frame()) {
  root <- local_results(env)
  db <- local_db(env)
  ingest_results(root, db)
  list(root = root, db = db)
}

# One results artifact of a dataset, read.
read_artifact <- function(root, dataset, file) {
  readRDS(file.path(root, "datasets", dataset, "artifacts", file))
}

fit_key_of <- function(db, method, where = "") {
  db_query(db, sprintf("SELECT fit_key FROM fits WHERE method = '%s' AND status = 'ok' %s ORDER BY fit_id", method, where))$fit_key[1]
}

fit_id_of <- function(db, key) db_query(db, sprintf("SELECT fit_id FROM fits WHERE fit_key = '%s'", key))$fit_id

read_table <- function(root, dataset, table) {
  as.data.frame(arrow::read_parquet(file.path(root, "datasets", dataset, "tables", paste0(table, ".parquet"))))
}

# Every table of two databases, rows ordered by all columns, ids included
# (meta holds the creation time, so it is skipped). Returns the names of the
# tables that differ.
differing_tables <- function(a, b) {
  ca <- DBI::dbConnect(RSQLite::SQLite(), a)
  cb <- DBI::dbConnect(RSQLite::SQLite(), b)
  on.exit({
    DBI::dbDisconnect(ca)
    DBI::dbDisconnect(cb)
  })
  tabs <- DBI::dbGetQuery(ca, "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%'")$name
  tabs <- setdiff(tabs, "meta")
  tabs[!vapply(tabs, function(t) {
    cols <- DBI::dbGetQuery(ca, sprintf("PRAGMA table_info(%s)", t))$name
    sql <- sprintf("SELECT * FROM %s ORDER BY %s", t, paste(DBI::dbQuoteIdentifier(ca, cols), collapse = ", "))
    identical(DBI::dbGetQuery(ca, sql), DBI::dbGetQuery(cb, sql))
  }, logical(1))]
}

# Crew workers are separate R processes, which cannot see a package that
# devtools::load_all() has loaded. Under load_all, install the source once per
# session into a temporary library and point the workers' R_LIBS at it.
worker_lib <- new.env()
local_worker_library <- function(env = parent.frame()) {
  if (!requireNamespace("pkgload", quietly = TRUE) || !pkgload::is_dev_package("sepsisfactorsdb")) {
    return(invisible(NULL))
  }
  if (is.null(worker_lib$path)) {
    lib <- tempfile("worker-lib-")
    dir.create(lib)
    utils::install.packages(
      pkgload::pkg_path(testthat::test_path()),
      lib = lib, repos = NULL, type = "source", quiet = TRUE,
      INSTALL_opts = c("--no-docs", "--no-test-load")
    )
    worker_lib$path <- lib
  }
  withr::local_envvar(R_LIBS = worker_lib$path, .local_envir = env)
  invisible(worker_lib$path)
}
