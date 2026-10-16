two_fits <- function(dir, flip = FALSE) {
  set.seed(1)
  genes <- sprintf("ENSG%05d", 1:60)
  L <- matrix(stats::rnorm(60 * 3), 60, 3, dimnames = list(genes, paste0("F", 1:3)))
  # the second fit: the same factors, permuted (and optionally sign-flipped), plus noise
  L2 <- L[, c(3, 1, 2)] * if (flip) c(-1, 1, 1) else 1
  L2 <- L2 + matrix(stats::rnorm(60 * 3, sd = 0.05), 60, 3)
  files <- file.path(dir, c("a.rds", "b.rds"))
  saveRDS(L, files[1])
  saveRDS(L2, files[2])
  files
}

test_that("Hungarian matching recovers a permutation of the factors", {
  files <- two_fits(withr::local_tempdir())
  sims <- pair_similarities(prepare_loadings(readRDS(files[1])), prepare_loadings(readRDS(files[2])))
  idx <- hungarian_match(sims$cosine)
  expect_equal(unname(idx[, "b"]), c(2L, 3L, 1L))
  expect_true(all(diag(sims$cosine[, idx[, "b"]]) > 0.99))
})

test_that("sign-ambiguous methods match on |cosine|", {
  files <- two_fits(withr::local_tempdir(), flip = TRUE)
  sims <- pair_similarities(prepare_loadings(readRDS(files[1])), prepare_loadings(readRDS(files[2])))
  expect_equal(unname(hungarian_match_abs(sims$cosine)[, "b"]), c(2L, 3L, 1L))
})

test_that("factor pairs carry matches, scores with runner-ups, null stats and stability", {
  files <- two_fits(withr::local_tempdir())
  pairs <- compute_factor_pairs(c("k1", "k2"), c(3, 3), files, sign_ambiguous = FALSE)
  expect_equal(pairs$fit_pairs$same_rank, 1L)
  expect_equal(nrow(pairs$matches), 3)
  expect_setequal(pairs$scores$metric, c("cosine", "pearson", "spearman"))
  expect_true(all(pairs$scores$margin[pairs$scores$metric == "cosine"] > 0.5))
  expect_equal(unique(pairs$null$metric), c("cosine", "pearson", "spearman"))
  expect_equal(nrow(pairs$stability), 6)
  expect_true(all(pairs$stability$cosine > 0.99))
  expect_equal(nrow(pairs$histograms), 3 * N_SIM_BINS)
})

test_that("the pair tables of the results output name fits and factors, not numbers", {
  files <- two_fits(withr::local_tempdir())
  tables <- pair_tables(compute_factor_pairs(c("k1", "k2"), c(3, 3), files), "D", "pca")
  expect_named(tables$fit_pairs, c("fit_a_key", "fit_b_key", "same_rank", "n_factors_a", "n_factors_b", "ari"))
  expect_named(tables$factor_matches, c("fit_a_key", "fit_b_key", "factor_a_index", "factor_b_index"))
  expect_named(tables$match_scores, c("fit_a_key", "fit_b_key", "factor_a_index", "metric", "value", "runner_up", "margin"))
  expect_named(tables$fit_pair_null, c("fit_a_key", "fit_b_key", "metric", "n", "mean", "sd", "median", "p95", "max"))
  expect_equal(unique(tables$similarity_histograms$method), "pca")
  expect_equal(tables$factor_matches$factor_b_index, c(2L, 3L, 1L))
  key <- paste(tables$match_scores$fit_a_key, tables$match_scores$factor_a_index)
  expect_equal(sum(!duplicated(key)), 3)
})

test_that("WGCNA pairs compare module assignments by ARI and gene Jaccard", {
  genes <- paste0("g", 1:60)
  a <- data.frame(gene = genes, module = rep(1:3, each = 20))
  b <- data.frame(gene = genes, module = rep(c(2L, 3L, 1L), each = 20))
  pairs <- compute_wgcna_pairs(list(a, b), c("w1", "w2"), c(6, 8))
  expect_equal(pairs$fit_pairs$ari, 1)
  expect_equal(pairs$fit_pairs$same_rank, 0L)
  expect_equal(unique(pairs$scores$metric), "jaccard")
  expect_equal(pairs$scores$value, rep(1, 3))
  expect_null(pairs$stability)
})

test_that("ICASSO clusters recurring components and scores them", {
  set.seed(2)
  genes <- sprintf("g%03d", 1:80)
  base <- matrix(stats::rnorm(80 * 2), 80, 2)
  fit <- function() sweep(base[, sample(2)], 2, sample(c(-1, 1), 2, TRUE), "*") + matrix(stats::rnorm(160, sd = 0.1), 80, 2)
  loadings <- stats::setNames(lapply(1:4, function(i) {
    m <- fit(); dimnames(m) <- list(genes, c("a", "b")); m
  }), as.character(1:4))
  res <- compute_icasso_clusters_from_loadings(loadings, 2)
  expect_equal(res$clusters$n_members, c(4L, 4L))
  expect_true(all(res$clusters$iq > 0.8))
  expect_equal(nrow(res$membership), 8)
})

test_that("the Ensembl map is one-to-one: one feature per gene, the highest-mean one", {
  mat <- rbind(p1 = c(1, 1, 1), p2 = c(5, 5, 5), p3 = c(2, 2, 2), p4 = c(9, 9, 9))
  colnames(mat) <- c("s1", "s2", "s3")
  fm <- data.frame(feature_id = c("p1", "p2", "p3", "p4"), ensembl = c("ENSG1.2", "ENSG1.7", "ENSG2", NA))
  map <- build_ensembl_map(list(dataset = list(ensembl_col = "ensembl", feature_id_col = "feature_id")), mat, fm = fm)
  expect_equal(map, c(p2 = "ENSG1", p3 = "ENSG2"))
  out <- remap_to_ensembl(mat, map)
  expect_equal(rownames(out), c("ENSG1", "ENSG2"))
  expect_equal(out["ENSG1", ], mat["p2", ])
  expect_snapshot(remap_to_ensembl(mat, c(p1 = "ENSG1", p2 = "ENSG1")), error = TRUE)
})

test_that("a ';'-separated multi-gene entry maps to its first gene", {
  mat <- matrix(1:6, 2, dimnames = list(c("p1", "p2"), c("s1", "s2", "s3")))
  fm <- data.frame(feature_id = c("p1", "p2"), ensembl = c("ENSG1;ENSG2", "ENSG3"))
  map <- build_ensembl_map(list(dataset = list(ensembl_col = "ensembl", feature_id_col = "feature_id")), mat, fm = fm)
  expect_equal(unname(map), c("ENSG1", "ENSG3"))
})

test_that("generic markers assign each gene to its strongest factor", {
  L <- matrix(c(5, 0, 0, 5, 1, 1), 3, 2, dimnames = list(c("a", "b", "c"), c("F1", "F2")), byrow = TRUE)
  m <- generic_pattern_markers(L)
  expect_equal(m$gene, c("a", "b", "c"))
  expect_equal(m$factor_index[1:2], c(1L, 2L))
})

test_that("an association scan tests numeric fields by Spearman and others by Kruskal-Wallis", {
  skip_if_not_installed("WGCNA")
  set.seed(3)
  scores <- matrix(stats::rnorm(40 * 2), 40, 2, dimnames = list(sprintf("s%02d", 1:40), c("c1", "c2")))
  meta <- data.frame(sample_id = rownames(scores), age = scores[, 1] + stats::rnorm(40, sd = 0.2),
                     group = rep(c("a", "b"), 20))
  res <- generic_association_scan(scores, meta)
  expect_setequal(res$test, c("spearman", "kruskal"))
  top <- res[res$field == "age" & res$component == "c1", ]
  expect_lt(top$p_value, 1e-6)
})
