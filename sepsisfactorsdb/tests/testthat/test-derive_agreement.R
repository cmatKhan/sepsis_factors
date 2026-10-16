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

test_that("the agreement order is by display order of the method, then rank, then fit id", {
  fits <- data.frame(
    fit_id = 1:5,
    method = c("wgcna", "pca", "nmf", "pca", "nmf"),
    rank = c(0, 3, 4, 2, 2),
    status = c("ok", "ok", "ok", "ok", "failed"),
    representative = c(1L, 1L, 1L, 1L, 1L)
  )
  expect_equal(agreement_fits(fits)$fit_id, c(4L, 2L, 3L, 1L))
})

test_that("method agreement covers every pair of methods at every top-N, PCA at each rank", {
  x <- toy_agreement()
  # pca (3 ranks) x nmf (1) + pca x wgcna (1) + nmf x wgcna (1), at each top-N
  expect_equal(derive_method_agreement(x$ctx), (3 + 3 + 1) * length(AGREEMENT_TOP_N))
  a <- db_query(x$toy$db, "
    SELECT f.method AS method_a, g.method AS method_b, m.* FROM method_agreement m
    JOIN fits f ON f.fit_id = m.fit_a JOIN fits g ON g.fit_id = m.fit_b")
  expect_equal(sort(unique(a$top_n)), AGREEMENT_TOP_N)
  expect_equal(sort(unique(a$n_factors_a[a$method_a == "pca"])), 1:3)
  # the method with the lower display order is always first
  ord <- c(pca = 1, spca = 2, ica = 3, nmf = 4, cogaps = 5, wgcna = 6)
  expect_true(all(ord[a$method_a] < ord[a$method_b]))
  # running the step again replaces the rows
  expect_equal(derive_method_agreement(x$ctx), nrow(a))
  expect_equal(db_count(x$toy$db, "method_agreement"), nrow(a))
})

test_that("an agreement row is the matched Jaccard of the top genes and the ARI of the clusters", {
  x <- toy_agreement()
  derive_method_agreement(x$ctx)
  r <- db_query(x$toy$db, "
    SELECT * FROM method_agreement
    WHERE fit_a = 1 AND fit_b = 2 AND n_factors_a = 2 AND top_n = 10")
  # top-N 10 is every gene of the toy dataset, so the sets are identical: Jaccard 1
  expect_equal(r$gene_jaccard, 1)
  con <- DBI::dbConnect(RSQLite::SQLite(), x$toy$db)
  withr::defer(DBI::dbDisconnect(con))
  blobs <- DBI::dbGetQuery(
    con,
    "SELECT jaccard_matrix, matches FROM method_agreement WHERE agreement_id = ?",
    params = list(r$agreement_id)
  )
  expect_equal(matrix(decode_f32(blobs$jaccard_matrix[[1]]), 2), matrix(1, 2, 2))
  expect_equal(decode_i32(blobs$matches[[1]])[1:2], 1:2)
  ca <- stats::cutree(stats::hclust(stats::dist(x$scores$pca[, 1:2])), 4)
  cb <- stats::cutree(stats::hclust(stats::dist(x$scores$nmf)), 4)
  expect_equal(r$sample_ari, mclust::adjustedRandIndex(ca, cb))
  expect_equal(r$n_shared, 8)
})

test_that("gene-set agreement depends on the top-N unless a side is a WGCNA module set", {
  x <- toy_agreement()
  derive_method_agreement(x$ctx)
  r <- db_query(x$toy$db, "
    SELECT a.method AS ma, b.method AS mb, m.top_n, m.gene_jaccard
    FROM method_agreement m JOIN fits a ON a.fit_id = m.fit_a JOIN fits b ON b.fit_id = m.fit_b")
  loadings_only <- r[r$mb != "wgcna", ]
  expect_equal(unique(loadings_only$gene_jaccard[loadings_only$top_n == 10]), 1)
  expect_true(all(r$gene_jaccard >= 0 & r$gene_jaccard <= 1))
  wg <- r[r$mb == "wgcna" & r$ma == "nmf", ]
  expect_length(unique(wg$gene_jaccard), 1)
})
