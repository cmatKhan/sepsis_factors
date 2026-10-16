test_that("loadings are projected by least squares on the shared genes", {
  b <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1), 4, 3, byrow = TRUE)
  rownames(b) <- letters[1:4]
  a <- b[, 1, drop = FALSE] * 2
  pr <- loadings_projection(a, b[c("a", "b", "c"), ])
  expect_equal(pr$n_genes, 3)
  expect_equal(unname(pr$projection[, 1]), c(2, 0, 0), tolerance = 1e-6)
  expect_null(loadings_projection(a, b[c("a", "b"), , drop = FALSE]))
})

test_that("greedy matching pairs rows and columns by |value|", {
  sim <- matrix(c(0.1, -0.9, 0.8, 0.2, 0.3, 0.1), 2)
  m <- greedy_match_abs(sim)
  expect_equal(m[, "a"], c(2, 1))
  expect_equal(m[, "b"], c(1, 2))
  expect_equal(nrow(greedy_match_abs(matrix(1:6, 3))), 2)
})

test_that("the gene biplot keeps the genes with the largest combined loading", {
  L <- cbind(c(0, 3, 1, 0), c(0, 4, 1, 0.5))
  rownames(L) <- letters[1:4]
  S <- matrix(c(1, -1, 2, -2), 2, 2, dimnames = list(c("s1", "s2"), NULL))
  d <- biplot_data(L, S, 1, 2, top_n = 2)
  expect_equal(d$arrows$gene, c("b", "c"))
  expect_equal(nrow(d$scores), 2)
  # the longest arrow reaches 80% of the score cloud's radius
  radius <- max(sqrt(d$scores$x^2 + d$scores$y^2))
  expect_equal(max(sqrt(d$arrows$x^2 + d$arrows$y^2)), 0.8 * radius)
})

test_that("the metadata biplot gives vectors for numbers and centroids for groups", {
  S <- cbind(c(1, 2, 3, 4, 5, 6), c(6, 5, 4, 3, 2, 1))
  rownames(S) <- paste0("s", 1:6)
  meta <- data.frame(
    sample_id = paste0("s", 1:6), age = c(10, 20, 30, 40, 50, 60),
    arm = rep(c("a", "b"), each = 3)
  )
  d <- biplot_metadata_data(S, meta, 1, 2)
  expect_equal(d$vectors$field, "age")
  expect_equal(d$vectors$dx, 1)
  expect_equal(d$vectors$dy, -1)
  expect_equal(d$centroids$level, c("a", "b"))
  expect_equal(d$centroids$x, c(2, 5))
  expect_null(biplot_metadata_data(S, meta[0, ], 1, 2))
})
