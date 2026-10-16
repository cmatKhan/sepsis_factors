test_that("blobs round trip, with NA kept", {
  x <- c(1.5, NA, -2.25, 1e-3)
  expect_equal(decode_f32(encode_f32(x)), x, tolerance = 1e-6)
  expect_equal(decode_i32(encode_i32(c(3L, NA, -4L))), c(3L, NA, -4L))
})

test_that("genes number the feature metadata in file order, an empty symbol is NA", {
  toy <- local_toy_db()
  save_artifact(
    toy,
    data.frame(gene_id = c("E1", "E2", "E3"), symbol = c("S1", "", "S3")),
    "fm.rds"
  )
  ctx <- toy_ctx(toy, toy_fits("pca"), dataset_files = c(feature_metadata = "fm.rds"))
  expect_equal(derive_genes(ctx), 3L)
  g <- DBI::dbGetQuery(toy$con, "SELECT * FROM genes ORDER BY gene_idx")
  expect_equal(g$gene_idx, 1:3)
  expect_equal(g$gene_id, c("E1", "E2", "E3"))
  expect_equal(g$symbol, c("S1", NA, "S3"))
  # running the step again leaves the same rows
  expect_equal(derive_genes(ctx), 3L)
  expect_equal(db_count(toy$db, "genes"), 3)
})

test_that("integer-valued feature ids are written without exponents", {
  expect_equal(as_id(c(1e5, 12, NA)), c("100000", "12", NA))
  expect_equal(as_id(factor(c("a", "b"))), c("a", "b"))
})

test_that("a gene that is not in the feature metadata is an error", {
  ctx <- list(dataset_id = "D", genes = toy_genes(3))
  expect_equal(gene_idx_of(ctx, c("G3", "G1"), "the loadings"), c(3L, 1L))
  expect_snapshot(gene_idx_of(ctx, c("G1", "NOT_A_GENE"), "the loadings"), error = TRUE)
  expect_error(gene_idx_of(ctx, "x", "the loadings"), class = "sepsisfactorsdb_unmapped_key")
})

test_that("a dataset without feature metadata cannot be numbered", {
  toy <- local_toy_db()
  ctx <- toy_ctx(toy, toy_fits("pca"))
  expect_snapshot(derive_genes(ctx), error = TRUE)
  save_artifact(toy, data.frame(gene_id = c("a", "a")), "fm.rds")
  ctx$dataset_files <- c(feature_metadata = "fm.rds")
  expect_error(derive_genes(ctx), class = "sepsisfactorsdb_invalid_results")
})

test_that("factor_top_genes holds the largest loadings on each side", {
  toy <- local_toy_db()
  L <- cbind(
    c(G1 = 0.5, G2 = -0.9, G3 = 0.1, G4 = 0.7, G5 = -0.2),
    c(G1 = 1, G2 = 2, G3 = 3, G4 = 4, G5 = 5)
  )
  save_artifact(toy, L, "loadings.rds")
  fit_files <- data.frame(fit_id = 1L, kind = "loadings", file = "loadings.rds")
  ctx <- toy_ctx(toy, toy_fits("ica"), fit_files, genes = toy_genes(5))
  expect_equal(derive_factor_top_genes(ctx, top = 2L), 6L)
  t <- DBI::dbGetQuery(toy$con, "SELECT * FROM factor_top_genes ORDER BY factor_index, side, rank")
  one_neg <- t[t$factor_index == 1 & t$side == "neg", ]
  expect_equal(one_neg$gene_idx, c(2L, 5L))
  expect_equal(one_neg$loading, c(-0.9, -0.2))
  one_pos <- t[t$factor_index == 1 & t$side == "pos", ]
  expect_equal(one_pos$gene_idx, c(4L, 1L))
  expect_equal(one_pos$rank, 1:2)
  # a factor with no negative loading has no negative side
  expect_false(any(t$factor_index == 2 & t$side == "neg"))
})

test_that("factor_top_genes keeps at most the top genes per side", {
  v <- seq(-1, 1, length.out = 200)
  rows <- top_gene_rows(v, seq_along(v), 1L, 1L, 50L)
  expect_equal(table(rows$side)[["pos"]], 50)
  expect_equal(table(rows$side)[["neg"]], 50)
  expect_equal(rows$gene_idx[rows$side == "pos"][1], 200)
})

test_that("fit_scores decode to the scores", {
  toy <- local_toy_db()
  S <- matrix(c(1.5, 2, NA, 4, 5, 6), 3, dimnames = list(c("a", "b", "c"), NULL))
  save_artifact(toy, S, "scores.rds")
  fit_files <- data.frame(fit_id = 1L, kind = "scores", file = "scores.rds")
  ctx <- toy_ctx(toy, toy_fits("nmf"), fit_files)
  expect_equal(derive_fit_scores(ctx), 1L)
  r <- DBI::dbGetQuery(toy$con, "SELECT * FROM fit_scores")
  expect_equal(jsonlite::fromJSON(r$row_ids), c("a", "b", "c"))
  expect_equal(r$n_factors, 2)
  expect_equal(matrix(decode_f32(r$data[[1]]), 3), unname(S), tolerance = 1e-6)
  saveRDS(unname(S), file.path(toy$dir, "scores.rds"))
  expect_error(derive_fit_scores(ctx), "no sample names")
})

test_that("fit_diag holds PCA's per-rank values only", {
  toy <- local_toy_db()
  save_artifact(toy, list(mse_by_rank = c(1, 0.6, 0.3), sdev = c(2, 1, 0.5)), "pca_diag.rds")
  save_artifact(toy, list(mse_by_rank = c(9, 9)), "nmf_diag.rds")
  fit_files <- data.frame(fit_id = 1:2, kind = "diag", file = c("pca_diag.rds", "nmf_diag.rds"))
  ctx <- toy_ctx(toy, toy_fits(c("pca", "nmf")), fit_files)
  expect_equal(derive_fit_diag(ctx), 6L)
  d <- DBI::dbGetQuery(toy$con, "SELECT * FROM fit_diag")
  expect_equal(unique(d$fit_id), 1L)
  expect_equal(d$value[d$name == "mse_by_rank"], c(1, 0.6, 0.3))
  expect_equal(d$value[d$name == "sdev"], c(2, 1, 0.5))
})

test_that("a WGCNA dendrogram decodes to the hclust pieces", {
  toy <- local_toy_db()
  x <- matrix(c(1, 2, 4, 8, 16), 5, dimnames = list(paste0("G", 5:1), NULL))
  h <- stats::hclust(stats::dist(x))
  d <- list(merge = h$merge, height = h$height, order = h$order, labels = h$labels)
  save_artifact(toy, d, "d.rds")
  fit_files <- data.frame(fit_id = 1L, kind = "dendrogram", file = "d.rds")
  ctx <- toy_ctx(toy, toy_fits("wgcna"), fit_files, genes = toy_genes(5))
  expect_equal(derive_wgcna_dendrograms(ctx), 1L)
  r <- DBI::dbGetQuery(toy$con, "SELECT * FROM wgcna_dendrograms")
  expect_equal(r$n, 5)
  expect_equal(matrix(decode_i32(r$merge[[1]]), ncol = 2), h$merge)
  expect_equal(decode_f32(r$height[[1]]), h$height, tolerance = 1e-6)
  expect_equal(decode_i32(r$ord[[1]]), h$order)
  expect_equal(decode_i32(r$gene_idx[[1]]), 5:1)
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
  expect_false(anyNA(out$low))
  expect_true(all(is.na(out$mean_diff)))
})

test_that("reduce_driver keeps significant and shared genes in PV mode", {
  res <- list(
    weighted_mean_stats = data.frame(
      gene = c("a", "b", "c", "d"),
      mean_diff = c(1, 2, 3, 4),
      welch_padj = c(0.01, 0.5, 0.2, NA)
    ),
    normalized_weights = c(a = 0.1, b = 0.2, c = 0.3, d = 0.4, z = 0.9),
    sig_genes = list(PV_significant_shared_genes = c("c", "z"))
  )
  out <- reduce_driver(res, "PV")
  expect_setequal(out$gene, c("a", "c", "z"))
  expect_equal(out$shared[order(out$gene)], c(0L, 1L, 1L))
  # a shared gene without statistics is added with NA statistics
  expect_true(is.na(out$mean_diff[out$gene == "z"]))
  expect_equal(out$weight[out$gene == "z"], 0.9)
  expect_true(all(is.na(out$low)))
  expect_false(anyNA(out$weight))
})

test_that("sample metadata is stored long with its R class", {
  toy <- local_toy_db()
  save_artifact(
    toy,
    data.frame(
      sample_id = c("S1", "S2"),
      age = c(30, 41),
      subject = c(1L, 2L),
      group = c("a", NA),
      stringsAsFactors = FALSE
    ),
    "meta.rds"
  )
  ctx <- toy_ctx(toy, toy_fits("pca"), dataset_files = c(sample_metadata = "meta.rds"))
  expect_equal(derive_sample_metadata(ctx), 6L)
  m <- DBI::dbGetQuery(toy$con, "SELECT * FROM sample_metadata WHERE sample_id = 'S2'")
  expect_equal(m$value[m$field == "age"], "41")
  expect_equal(m$type[m$field == "age"], "numeric")
  expect_equal(m$type[m$field == "subject"], "integer")
  expect_equal(m$type[m$field == "group"], "character")
  expect_false("sample_id" %in% m$field)
  # a dataset without sample metadata has no rows
  ctx$dataset_files <- character()
  expect_equal(derive_sample_metadata(ctx), 0L)
  expect_equal(db_count(toy$db, "sample_metadata"), 0)
})

test_that("gene variance matches genes by id, then by symbol", {
  toy <- local_toy_db()
  vp <- list(gene_summary = data.frame(
    gene = c("G1", "SYM2", "G3"),
    time_all = c(0.1, 0.2, 0.3),
    factor_gene = c(TRUE, FALSE, TRUE)
  ))
  save_artifact(toy, vp, "vp.rds")
  genes <- toy_genes(3, symbols = c("SYM1", "SYM2", "SYM3"))
  files <- c(variance_partition = "vp.rds")
  ctx <- toy_ctx(toy, toy_fits("pca"), dataset_files = files, genes = genes)
  expect_equal(derive_gene_variance(ctx), 3L)
  g <- DBI::dbGetQuery(toy$con, "SELECT * FROM gene_variance ORDER BY gene_idx")
  expect_equal(g$gene_idx, 1:3)
  expect_equal(g$factor_gene, c(1, 0, 1))
  expect_equal(g$time_all, c(0.1, 0.2, 0.3))
  vp$gene_summary$gene[1] <- "nowhere"
  save_artifact(toy, vp, "vp.rds")
  expect_error(derive_gene_variance(ctx), class = "sepsisfactorsdb_unmapped_key")
})

test_that("projection values hold weights, p values and r squared", {
  p <- matrix(c(1, 2, 3, 4, 5, 6), 2, dimnames = list(c("c1", "c2"), c("s1", "s2", "s3")))
  x <- list(projection = p, pval = p / 10, r_squared = c(s3 = 0.3, s1 = 0.1, s2 = 0.2))
  e <- encode_projection(x, "p.rds")
  expect_equal(jsonlite::fromJSON(e$row_ids), c("c1", "c2"))
  expect_equal(jsonlite::fromJSON(e$col_ids), c("s1", "s2", "s3"))
  v <- decode_f32(e$data)
  expect_length(v, 6 + 6 + 3)
  expect_equal(v[1:6], as.vector(p), tolerance = 1e-6)
  expect_equal(v[7:12], as.vector(p) / 10, tolerance = 1e-6)
  # r squared follows the column order of the projection
  expect_equal(v[13:15], c(0.1, 0.2, 0.3), tolerance = 1e-6)
})

test_that("a projection file without p values or r squared stores NA", {
  p <- matrix(1:4, 2, dimnames = list(c("a", "b"), c("x", "y")))
  v <- decode_f32(encode_projection(list(projection = p), "p.rds")$data)
  expect_false(anyNA(v[1:4]))
  expect_true(all(is.na(v[5:10])))
})

test_that("a projection without row and column names is refused", {
  expect_snapshot(encode_projection(list(projection = matrix(1:4, 2)), "p.rds"), error = TRUE)
})

test_that("the derive step needs the dataset to be loaded first", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "datasets", "TOY1", "tables"), recursive = TRUE)
  db <- local_db()
  DBI::dbDisconnect(open_stability_db(db))
  expect_error(derive_dataset(root, db, "TOY1"), class = "sepsisfactorsdb_unmapped_key")
})
