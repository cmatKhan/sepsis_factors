test_that("methods are listed in display order, unknown ones last", {
  expect_equal(order_methods(c("wgcna", "nmf", "pca", NA)), c("pca", "nmf", "wgcna"))
  expect_equal(order_methods(c("zzz", "pca")), c("pca", "zzz"))
  expect_error(check_metric("euclid"), "metric must be one of")
})

test_that("the database lists datasets, curves and optimal fits", {
  con <- local_real_db()
  ds <- list_datasets(con)
  expect_gt(length(ds), 0)
  for (id in intersect(c("EARLI", "ANEMONES"), ds)) {
    cv <- dataset_curves(con, id)
    expect_gt(nrow(cv), 0)
    expect_gt(nrow(optimal_fits(cv, "stable", 0.9)), 0)
    expect_gt(nrow(optimal_fits(cv, "max_r2")), 0)
    a <- method_pairwise_agreement(con, optimal_fits(cv))
    expect_contains(names(a), c("gene_jaccard", "sample_ari"))
  }
})

test_that("cross-dataset similarity builds a matrix", {
  con <- local_real_db()
  for (m in c("pca", "nmf")) {
    for (k in c("r2", "gene_space")) {
      expect_true(is.matrix(similarity_matrix(cross_dataset_similarity(con, m, k))))
    }
  }
})
