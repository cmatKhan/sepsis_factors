# A fresh example results output (and a path for a scratch database) that is
# removed when the calling test finishes.
local_results <- function(env = parent.frame()) {
  path <- withr::local_tempdir("results-", .local_envir = env)
  example_results(file.path(path, "res"))
}

# A copy of an example results output that a test may edit.
copy_results <- function(base, env = parent.frame()) {
  dir <- withr::local_tempdir("results-", .local_envir = env)
  file.copy(base, dir, recursive = TRUE)
  file.path(dir, basename(base))
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

read_table <- function(root, dataset, table) {
  as.data.frame(
    arrow::read_parquet(file.path(root, "datasets", dataset, "tables", paste0(table, ".parquet")))
  )
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
  tabs <- DBI::dbGetQuery(
    ca,
    "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%'"
  )$name
  tabs <- setdiff(tabs, "meta")
  tabs[!vapply(tabs, function(t) {
    cols <- DBI::dbGetQuery(ca, sprintf("PRAGMA table_info(%s)", t))$name
    sql <- sprintf(
      "SELECT * FROM %s ORDER BY %s",
      t,
      paste(DBI::dbQuoteIdentifier(ca, cols), collapse = ", ")
    )
    identical(DBI::dbGetQuery(ca, sql), DBI::dbGetQuery(cb, sql))
  }, logical(1))]
}


# A small database for testing one derive step on hand-built input: the schema
# with foreign keys off, and one dataset "D". `dir` also holds the artifact
# files the context reads.
local_toy_db <- function(env = parent.frame()) {
  dir <- withr::local_tempdir("toy-", .local_envir = env)
  db <- file.path(dir, "stability.sqlite")
  con <- open_stability_db(db)
  withr::defer(DBI::dbDisconnect(con), envir = env)
  DBI::dbExecute(con, "PRAGMA foreign_keys = OFF")
  DBI::dbAppendTable(
    con,
    "datasets",
    data.frame(
      dataset_id = "D",
      sample_id_col = "sample_id",
      feature_id_col = "gene_id",
      symbol_col = "symbol"
    )
  )
  list(con = con, db = db, dir = dir)
}

# Fits table rows for `local_toy_db()`: one row per method, ids from 1.
toy_fits <- function(methods, representative = 1L, n_factors = 2L, mse = NA_real_) {
  data.frame(
    fit_id = seq_along(methods),
    fit_key = paste0(methods, "_", seq_along(methods)),
    dataset_id = "D",
    method = methods,
    rank = n_factors,
    seed = 1L,
    bootstrap = 0L,
    representative = representative,
    mse = mse,
    n_factors = n_factors,
    status = "ok"
  )
}

# The context a derive step takes, around a `local_toy_db()`. `fit_files` is a
# data frame (`fit_id`, `kind`, `file`) of artifacts saved under `toy$dir`.
toy_ctx <- function(toy, fits, fit_files = NULL, dataset_files = character(), genes = NULL) {
  DBI::dbAppendTable(toy$con, "fits", fits[, c(
    "fit_id", "fit_key", "dataset_id", "method", "rank", "seed", "bootstrap", "representative",
    "mse", "n_factors", "status"
  )])
  if (is.null(fit_files)) {
    fit_files <- data.frame(fit_id = integer(), kind = character(), file = character())
  }
  list(
    con = toy$con,
    db_path = toy$db,
    artifact_base = toy$db,
    dataset_id = "D",
    dataset = DBI::dbGetQuery(toy$con, "SELECT * FROM datasets WHERE dataset_id = 'D'"),
    results_artifacts = toy$dir,
    fits = fits,
    all_fits = fits,
    all_fit_files = fit_files,
    fit_files = fit_files,
    dataset_files = dataset_files,
    genes = genes
  )
}

save_artifact <- function(toy, x, file) {
  saveRDS(x, file.path(toy$dir, file))
  invisible(file)
}

# The gene lookup of genes G1..Gn, as `load_gene_lookup()` returns it.
toy_genes <- function(n, symbols = NULL) {
  list(
    lookup = stats::setNames(seq_len(n), paste0("G", seq_len(n))),
    by_symbol = if (is.null(symbols)) {
      stats::setNames(integer(), character())
    } else {
      stats::setNames(seq_along(symbols), symbols)
    }
  )
}

# A part file of a parallel build: dataset `dataset_id` with `n_fits` fits, two
# factors each, one pair of the first two fits and, if `agreement`, one
# agreement row between them. Every id starts from 1, as in a part.
toy_part <- function(path, dataset_id, n_fits = 2L, agreement = FALSE) {
  con <- open_stability_db(path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbAppendTable(con, "datasets", data.frame(dataset_id = dataset_id))
  DBI::dbExecute(
    con,
    "INSERT OR IGNORE INTO methods VALUES ('pca', 'linear', 1, 1, 'PCA', '#000000', 1, 'b')"
  )
  fits <- data.frame(
    fit_id = seq_len(n_fits),
    fit_key = paste0(dataset_id, "_fit", seq_len(n_fits)),
    dataset_id = dataset_id,
    method = "pca",
    status = "ok"
  )
  DBI::dbAppendTable(con, "fits", fits)
  DBI::dbAppendTable(
    con,
    "factors",
    data.frame(
      factor_id = seq_len(2 * n_fits),
      fit_id = rep(seq_len(n_fits), each = 2),
      factor_index = rep(1:2, n_fits)
    )
  )
  DBI::dbAppendTable(
    con,
    "fit_pairs",
    data.frame(fit_pair_id = 1L, fit_a = 1L, fit_b = 2L, same_rank = 1L)
  )
  if (agreement) {
    DBI::dbAppendTable(
      con,
      "method_agreement",
      data.frame(
        agreement_id = 1L,
        dataset_id = dataset_id,
        fit_a = 1L,
        n_factors_a = 2L,
        fit_b = 2L,
        n_factors_b = 2L,
        top_n = 10L
      )
    )
  }
  invisible(path)
}

# Three representative fits over genes G1..G10 and eight samples; a hand-made
# database and artifacts instead of a loaded dataset.
toy_agreement <- function(env = parent.frame()) {
  withr::local_seed(4, .local_envir = env)
  toy <- local_toy_db(env)
  genes <- paste0("G", 1:10)
  samples <- paste0("S", 1:8)
  loadings <- function(k) matrix(stats::rnorm(10 * k), 10, k, dimnames = list(genes, NULL))
  scores <- function(k) matrix(stats::rnorm(8 * k), 8, k, dimnames = list(samples, NULL))
  save_artifact(toy, loadings(3), "pca_l.rds")
  save_artifact(toy, scores(3), "pca_s.rds")
  save_artifact(toy, loadings(2), "nmf_l.rds")
  save_artifact(toy, scores(2), "nmf_s.rds")
  save_artifact(toy, scores(3), "wgcna_s.rds")
  fit_files <- data.frame(
    fit_id = c(1L, 1L, 2L, 2L, 3L),
    kind = c("loadings", "scores", "loadings", "scores", "scores"),
    file = c("pca_l.rds", "pca_s.rds", "nmf_l.rds", "nmf_s.rds", "wgcna_s.rds")
  )
  ctx <- toy_ctx(toy, toy_fits(c("pca", "nmf", "wgcna")), fit_files, genes = toy_genes(10))
  DBI::dbAppendTable(
    toy$con, "fit_diag", data.frame(fit_id = 1L, name = "mse_by_rank", idx = 1:3, value = 3:1)
  )
  DBI::dbAppendTable(
    toy$con, "gene_modules",
    data.frame(fit_id = 3L, gene = genes[1:6], module = rep(1:2, each = 3))
  )
  list(toy = toy, ctx = ctx, scores = list(
    pca = readRDS(file.path(toy$dir, "pca_s.rds")), nmf = readRDS(file.path(toy$dir, "nmf_s.rds"))
  ))
}
