test_that("prepare_loadings gives unit-norm columns for every metric", {
  set.seed(1)
  m <- matrix(rnorm(60), 20, 3, dimnames = list(paste0("g", 1:20), NULL))
  prep <- prepare_loadings(m)
  expect_named(prep, c("cosine", "pearson", "spearman"))
  for (p in prep) expect_equal(unname(colSums(p^2)), rep(1, 3))
})

test_that("a fit matches itself, up to sign, under Hungarian matching", {
  set.seed(2)
  m <- matrix(rnorm(100), 25, 4, dimnames = list(paste0("g", 1:25), NULL))
  flipped <- sweep(m[, c(3, 1, 4, 2)], 2, c(1, -1, 1, -1), "*")
  prep_a <- prepare_loadings(m)
  prep_b <- prepare_loadings(flipped)
  sim <- pair_similarities(prep_a, prep_b)$cosine
  expect_equal(unname(hungarian_match_abs(sim)[, "b"]), c(2L, 4L, 1L, 3L))
  expect_equal(unname(hungarian_match(abs(sim))[, "b"]), c(2L, 4L, 1L, 3L))
})

test_that("pair_similarities needs shared genes", {
  a <- prepare_loadings(matrix(1:6, 3, 2, dimnames = list(c("a", "b", "c"), NULL)))
  b <- prepare_loadings(matrix(1:6, 3, 2, dimnames = list(c("x", "y", "z"), NULL)))
  expect_null(pair_similarities(a, b))
})
