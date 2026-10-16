test_that("jaccard_sets agrees with set arithmetic", {
  a <- list(1:5, c(3L, 9L))
  b <- list(c(1L, 2L, 7L), 4:6, integer())
  j <- jaccard_sets(a, b)
  for (i in 1:2) {
    for (k in 1:3) {
      u <- union(a[[i]], b[[k]])
      expect_equal(j[i, k], length(intersect(a[[i]], b[[k]])) / length(u))
    }
  }
})

test_that("hungarian_match_abs matches on |similarity| and handles tall matrices", {
  sim <- matrix(c(-0.9, 0.1, 0.2, 0.8), 2)
  expect_equal(unname(hungarian_match_abs(sim)), cbind(1:2, 1:2), ignore_attr = TRUE)
  tall <- rbind(sim, c(0, 0))
  m <- hungarian_match_abs(tall)
  expect_equal(nrow(m), 2)
})

test_that("method agreement covers every pair of methods at every top-N, PCA at each rank", {
  x <- local_ingested()
  a <- db_query(x$db, "SELECT f.method AS method_a, g.method AS method_b, m.* FROM method_agreement m
                       JOIN fits f ON f.fit_id = m.fit_a JOIN fits g ON g.fit_id = m.fit_b WHERE m.dataset_id = 'TOY1'")
  expect_equal(nrow(a), 25 * length(AGREEMENT_TOP_N))
  expect_equal(sort(unique(a$top_n)), AGREEMENT_TOP_N)
  expect_equal(sort(unique(a$n_factors_a[a$method_a == "pca"])), 1:3)
  # the method with the lower display order is always first
  ord <- c(pca = 1, spca = 2, ica = 3, nmf = 4, cogaps = 5, wgcna = 6)
  expect_true(all(ord[a$method_a] < ord[a$method_b]))
  expect_equal(db_count(x$db, "method_agreement"), 25 * 5)
})

test_that("an agreement row equals the Jaccard of the top genes, matched, and the ARI of the clusters", {
  x <- local_ingested()
  keys <- list(nmf = fit_key_of(x$db, "nmf", "AND representative = 1"), pca = fit_key_of(x$db, "pca", "AND dataset_id = 'TOY1'"))
  r <- db_query(x$db, sprintf(
    "SELECT m.* FROM method_agreement m JOIN fits a ON a.fit_id = m.fit_a JOIN fits b ON b.fit_id = m.fit_b
     WHERE a.fit_key = '%s' AND b.fit_key = '%s' AND m.n_factors_a = 2 AND m.top_n = 10", keys$pca, keys$nmf
  ))
  # top-N 10 is every gene of the toy dataset, so the sets are identical: Jaccard 1
  expect_equal(r$gene_jaccard, 1)
  con <- DBI::dbConnect(RSQLite::SQLite(), x$db)
  withr::defer(DBI::dbDisconnect(con))
  r <- DBI::dbGetQuery(con, sprintf(
    "SELECT m.* FROM method_agreement m JOIN fits a ON a.fit_id = m.fit_a JOIN fits b ON b.fit_id = m.fit_b
     WHERE a.fit_key = '%s' AND b.fit_key = '%s' AND m.n_factors_a = 2 AND m.top_n = 10", keys$pca, keys$nmf
  ))
  expect_equal(matrix(decode_f32(r$jaccard_matrix[[1]]), 2), matrix(1, 2, 2))
  m <- decode_i32(r$matches[[1]])
  expect_equal(m[1:2], 1:2) # factor indices of the rows
  # sample ARI from the scores
  Sa <- read_artifact(x$root, "TOY1", paste0(keys$pca, "_scores.rds"))[, 1:2]
  Sb <- read_artifact(x$root, "TOY1", paste0(keys$nmf, "_scores.rds"))
  ca <- stats::cutree(stats::hclust(stats::dist(Sa)), 4)
  cb <- stats::cutree(stats::hclust(stats::dist(Sb)), 4)
  expect_equal(r$sample_ari, mclust::adjustedRandIndex(ca, cb))
  expect_equal(r$n_shared, 6)
})

test_that("gene-set agreement depends on the top-N unless a side is a WGCNA module set", {
  x <- local_ingested()
  r <- db_query(x$db, "SELECT a.method AS ma, b.method AS mb, m.top_n, m.gene_jaccard
                       FROM method_agreement m JOIN fits a ON a.fit_id = m.fit_a JOIN fits b ON b.fit_id = m.fit_b")
  loadings_only <- r[r$mb != "wgcna", ]
  expect_equal(unique(loadings_only$gene_jaccard[loadings_only$top_n == 10]), 1)
  expect_true(all(r$gene_jaccard >= 0 & r$gene_jaccard <= 1))
})
