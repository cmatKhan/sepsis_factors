test_that("blobs round trip, with NA kept", {
  x <- c(1.5, NA, -2.25, 1e-3)
  expect_equal(decode_f32(encode_f32(x)), x, tolerance = 1e-6)
  expect_equal(decode_i32(encode_i32(c(3L, NA, -4L))), c(3L, NA, -4L))
})

test_that("genes number the feature metadata", {
  x <- local_ingested()
  g <- db_query(x$db, "SELECT * FROM genes WHERE dataset_id = 'TOY1' ORDER BY gene_idx")
  expect_equal(g$gene_idx, 1:10)
  expect_equal(g$symbol, paste0("SYM", 1:10))
})

test_that("the optional family columns arrive", {
  x <- local_ingested()
  d <- db_query(x$db, "SELECT family, family_position, hf_collection FROM datasets WHERE dataset_id = 'TOY1'")
  expect_equal(d$family, "TOY1")
  expect_equal(d$hf_collection, "toy/collection")
})

test_that("factor_top_genes holds the largest loadings on each side", {
  x <- local_ingested()
  key <- fit_key_of(x$db, "ica", "AND bootstrap = 0")
  L <- read_artifact(x$root, "TOY1", paste0(key, "_loadings.rds"))
  t <- db_query(x$db, sprintf("SELECT t.* FROM factor_top_genes t JOIN fits f USING (fit_id) WHERE f.fit_key = '%s'", key))
  one <- t[t$factor_index == 1 & t$side == "pos", ]
  expect_equal(one$gene_idx[1], unname(which.max(L[, 1])))
  expect_equal(one$loading[1], max(L[, 1]))
  expect_equal(nrow(one), sum(L[, 1] > 0))
  neg <- t[t$factor_index == 1 & t$side == "neg", ]
  expect_true(all(neg$loading < 0))
  expect_equal(neg$loading[1], min(L[, 1]))
})

test_that("factor_top_genes keeps at most the top genes per side", {
  v <- seq(-1, 1, length.out = 200)
  rows <- top_gene_rows(v, seq_along(v), 1L, 1L, 50L)
  expect_equal(table(rows$side)[["pos"]], 50)
  expect_equal(table(rows$side)[["neg"]], 50)
  expect_equal(rows$gene_idx[rows$side == "pos"][1], 200)
})

test_that("fit_scores decode to the scores", {
  x <- local_ingested()
  key <- fit_key_of(x$db, "nmf")
  S <- read_artifact(x$root, "TOY1", paste0(key, "_scores.rds"))
  r <- db_query(x$db, sprintf("SELECT s.* FROM fit_scores s JOIN fits f USING (fit_id) WHERE f.fit_key = '%s'", key))
  expect_equal(jsonlite::fromJSON(r$row_ids), rownames(S))
  expect_equal(r$n_factors, ncol(S))
  con <- DBI::dbConnect(RSQLite::SQLite(), x$db)
  withr::defer(DBI::dbDisconnect(con))
  blob <- DBI::dbGetQuery(con, sprintf("SELECT data FROM fit_scores WHERE fit_id = %d", r$fit_id))$data[[1]]
  expect_equal(matrix(decode_f32(blob), nrow(S)), unname(S), tolerance = 1e-6)
  # every fit with scores has a row, WGCNA's eigengenes included
  n <- db_query(x$db, "SELECT COUNT(*) AS n FROM fit_artifacts WHERE kind = 'scores'")$n
  expect_equal(db_count(x$db, "fit_scores"), n)
})

test_that("fit_diag holds PCA's per-rank values only", {
  x <- local_ingested()
  d <- db_query(x$db, "SELECT f.method, d.name, d.idx, d.value FROM fit_diag d JOIN fits f USING (fit_id)
                       WHERE f.dataset_id = 'TOY1'")
  expect_equal(unique(d$method), "pca")
  expect_equal(d$value[d$name == "mse_by_rank"], c(1, 0.6, 0.3))
  expect_equal(d$value[d$name == "sdev"], c(2, 1, 0.5))
})

test_that("a WGCNA dendrogram decodes to the hclust pieces", {
  x <- local_ingested()
  key <- fit_key_of(x$db, "wgcna", "AND representative = 1")
  d <- read_artifact(x$root, "TOY1", paste0(key, "_dendrogram.rds"))
  con <- DBI::dbConnect(RSQLite::SQLite(), x$db)
  withr::defer(DBI::dbDisconnect(con))
  r <- DBI::dbGetQuery(con, sprintf("SELECT w.* FROM wgcna_dendrograms w JOIN fits f USING (fit_id) WHERE f.fit_key = '%s'", key))
  expect_equal(r$n, 10)
  expect_equal(matrix(decode_i32(r$merge[[1]]), ncol = 2), d$merge)
  expect_equal(decode_f32(r$height[[1]]), d$height, tolerance = 1e-6)
  expect_equal(decode_i32(r$ord[[1]]), d$order)
  expect_equal(decode_i32(r$gene_idx[[1]]), 1:10)
})

test_that("driver genes are reduced per mode", {
  x <- local_ingested()
  d <- db_query(x$db, "SELECT p.mode, g.* FROM driver_genes g JOIN pattern_drivers p USING (driver_id)")
  ci <- d[d$mode == "CI", ]
  pv <- d[d$mode == "PV", ]
  expect_true(all(c(1, 2) %in% ci$gene_idx[ci$shared == 1]))
  expect_lte(nrow(ci), 10)
  expect_false(anyNA(ci$low))
  expect_true(all(is.na(ci$mean_diff)))
  expect_setequal(pv$gene_idx, 1:3)
  expect_equal(sort(pv$gene_idx[pv$shared == 1]), 1:2)
  expect_false(anyNA(pv$weight))
  expect_true(all(is.na(pv$low)))
})

test_that("reduce_driver keeps the top CI genes and every shared gene", {
  set.seed(1)
  genes <- paste0("g", 1:100)
  mid <- seq(-5, 5, length.out = 100)
  res <- list(
    weighted_mean_ci = data.frame(low = mid - 1, high = mid + 1, gene = genes),
    normalized_weights = stats::setNames(stats::rnorm(100), genes),
    sig_genes = list(significant_shared_genes = c("g50", "g51"))
  )
  out <- reduce_driver(res, "CI")
  expect_equal(nrow(out), 27)
  expect_true(all(c("g1", "g100", "g50", "g51") %in% out$gene))
  expect_equal(sum(out$shared), 2)
})

test_that("sample metadata is stored long with its R class", {
  x <- local_ingested()
  m <- db_query(x$db, "SELECT * FROM sample_metadata WHERE dataset_id = 'TOY1' AND sample_id = 'TOY1_S2'")
  expect_equal(m$value[m$field == "age"], "41")
  expect_equal(m$type[m$field == "age"], "numeric")
  expect_equal(m$type[m$field == "subject"], "integer")
  expect_equal(m$type[m$field == "group"], "character")
  expect_false("sample_id" %in% m$field)
})

test_that("gene variance matches genes by id, then by symbol", {
  root <- local_results()
  vp <- list(gene_summary = data.frame(
    gene = c("ENSG00000000001", "SYM2", "SYM3"),
    time_all = c(0.1, 0.2, 0.3),
    factor_gene = c(TRUE, FALSE, TRUE)
  ))
  saveRDS(vp, file.path(root, "datasets", "TOY1", "artifacts", "variance_partition.rds"))
  edit_table(root, "TOY1", "dataset_artifacts", function(df) {
    rbind(df, data.frame(dataset_id = "TOY1", kind = "variance_partition", path = "variance_partition.rds"))
  })
  db <- local_db()
  ingest_results(root, db)
  g <- db_query(db, "SELECT * FROM gene_variance ORDER BY gene_idx")
  expect_equal(g$gene_idx, 1:3)
  expect_equal(g$factor_gene, c(1, 0, 1))
  expect_equal(g$time_all, c(0.1, 0.2, 0.3))
})

test_that("a gene that is not in the feature metadata is an error", {
  root <- local_results()
  f <- list.files(file.path(root, "datasets", "TOY1", "artifacts"), pattern = "_loadings\\.rds$", full.names = TRUE)[1]
  L <- readRDS(f)
  rownames(L)[1] <- "NOT_A_GENE"
  saveRDS(L, f)
  expect_snapshot(ingest_results(root, local_db()), error = TRUE, transform = scrub_tmp)
  expect_error(ingest_results(root, local_db()), class = "sepsisfactorsdb_unmapped_key")
})

test_that("a dataset without feature metadata cannot be numbered", {
  root <- local_results()
  edit_table(root, "TOY1", "dataset_artifacts", function(df) df[df$kind != "feature_metadata", ])
  expect_snapshot(ingest_results(root, local_db()), error = TRUE, transform = scrub_tmp)
})

test_that("projection values hold weights, p values and r squared", {
  x <- local_ingested()
  path <- db_query(x$db, "SELECT path FROM projections")$path
  p <- readRDS(resolve_artifact(path, x$db))
  con <- DBI::dbConnect(RSQLite::SQLite(), x$db)
  withr::defer(DBI::dbDisconnect(con))
  r <- DBI::dbGetQuery(con, "SELECT * FROM projection_values")
  expect_equal(jsonlite::fromJSON(r$row_ids), rownames(p$projection))
  expect_equal(jsonlite::fromJSON(r$col_ids), colnames(p$projection))
  v <- decode_f32(r$data[[1]])
  expect_length(v, 12 + 12 + 4)
  expect_equal(v[1:12], as.vector(p$projection), tolerance = 1e-6)
  expect_equal(v[13:24], as.vector(p$pval), tolerance = 1e-6)
  expect_equal(v[25:28], unname(p$r_squared), tolerance = 1e-6)
})

test_that("a projection file without p values or r squared stores NA", {
  root <- local_results()
  f <- list.files(file.path(root, "cross_dataset", "artifacts"), full.names = TRUE)
  p <- readRDS(f)
  saveRDS(list(projection = p$projection), f)
  db <- local_db()
  ingest_results(root, db)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  withr::defer(DBI::dbDisconnect(con))
  v <- decode_f32(DBI::dbGetQuery(con, "SELECT data FROM projection_values")$data[[1]])
  expect_true(all(is.na(v[13:28])))
  expect_false(anyNA(v[1:12]))
})

test_that("a projection without row and column names is refused", {
  expect_snapshot(encode_projection(list(projection = matrix(1:4, 2)), "p.rds"), error = TRUE)
})

test_that("the derive step is idempotent", {
  x <- local_ingested()
  tables <- c(
    "genes", "factor_top_genes", "fit_scores", "fit_diag", "wgcna_dendrograms", "driver_genes",
    "sample_metadata", "fit_summary", "factor_association", "method_agreement", "projection_values"
  )
  before <- vapply(tables, db_count, numeric(1), db = x$db)
  ingest_results(x$root, x$db)
  expect_equal(vapply(tables, db_count, numeric(1), db = x$db), before)
  expect_true(all(before > 0))
})

test_that("the derive step needs the dataset to be loaded first", {
  root <- local_results()
  db <- local_db()
  DBI::dbDisconnect(open_stability_db(db))
  expect_error(derive_dataset(root, db, "TOY1"), class = "sepsisfactorsdb_unmapped_key")
})

test_that("a new database records its provenance", {
  x <- local_ingested()
  m <- db_query(x$db, "SELECT key, value FROM meta")
  expect_setequal(m$key, c("schema_version", "created_at", "writer", "writer_version"))
  expect_equal(m$value[m$key == "schema_version"], as.character(SCHEMA_VERSION))
})

test_that("methods carry their display metadata", {
  x <- local_ingested()
  m <- db_query(x$db, "SELECT * FROM methods ORDER BY display_order")
  expect_equal(m$method, c("pca", "spca", "ica", "nmf", "cogaps", "wgcna"))
  expect_equal(m$label, c("PCA", "sPCA", "ICA", "NMF", "CoGAPS", "WGCNA"))
  expect_true(all(grepl("^#[0-9A-F]{6}$", m$color)))
})
