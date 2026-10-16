db <- open_fixture()

test_that("the agreement grid is the one the database stored", {
  expect_equal(agreement_grid(db, "TOY1"), c(10, 25, 50, 100, 200))
  expect_equal(agreement_grid(db, "NOPE"), numeric(0))
})

test_that("a stored agreement row decodes into a labelled matrix and matches", {
  row <- data.frame(gene_jaccard = 0.4, sample_ari = 0.1, n_shared = 6L)
  # rows are factors 3, 1, 2 of fit a; columns are factors 5 and 7 of fit b
  row$jaccard_matrix <- list(encode_f32(c(0.1, 0.2, 0.3, 0.9, 0.5, 0.4)))
  # factor indices of the rows and columns, then the matched row and column positions
  row$matches <- list(encode_i32(c(3, 1, 2, 5, 7, 2, 1, 2, 1)))
  out <- decode_agreement(
    row, 3, 2, c(`1` = "A1", `2` = "A2", `3` = "A3"), c(`5` = "B5", `7` = "B7")
  )
  expect_equal(rownames(out$matrix), c("A3", "A1", "A2"))
  expect_equal(colnames(out$matrix), c("B5", "B7"))
  expect_equal(out$matrix["A1", "B5"], 0.2, tolerance = 1e-6)
  # matched pairs: row 2 (A1) with column 2 (B7) = 0.5, and row 1 (A3) with column 1 (B5) = 0.1
  expect_equal(out$matches$a, c("A1", "A3"))
  expect_equal(out$matches$b, c("B7", "B5"))
  expect_equal(out$matches$jaccard, c(0.5, 0.1), tolerance = 1e-6)
  expect_equal(out$mean, 0.4)
})

test_that("pairwise agreement reads the stored pairs of the optimal fits", {
  optimal <- data.frame(
    method = c("pca", "spca", "ica"), fit_id = c(1, 2, 5), n_factors = c(3, 2, 2)
  )
  a <- method_pairwise_agreement(db, optimal, 50)
  expect_equal(a$methods, optimal$method)
  expect_equal(dim(a$gene_jaccard), c(3, 3))
  expect_equal(diag(a$gene_jaccard), c(1, 1, 1), ignore_attr = TRUE)
  expect_equal(a$gene_jaccard, t(a$gene_jaccard))
  stored <- db_query(
    db,
    "SELECT gene_jaccard, sample_ari FROM method_agreement
     WHERE fit_a = 1 AND n_factors_a = 3 AND fit_b = 2 AND n_factors_b = 2 AND top_n = 50"
  )
  expect_equal(a$gene_jaccard["pca", "spca"], stored$gene_jaccard)
  expect_equal(a$sample_ari["pca", "spca"], stored$sample_ari)
  pair <- a$pairs[["pca|spca"]]
  expect_equal(dim(pair$matrix), c(3, 2))
  expect_equal(nrow(pair$matches), 2)
  expect_false(is.unsorted(rev(pair$matches$jaccard)))
  expect_true(all(pair$matches$a %in% fit_factors(db, 1)$label))
  # a top-N that was not stored has no pairs
  expect_length(method_pairwise_agreement(db, optimal, 7)$pairs, 0)
})

test_that("the association of factors with metadata reads the stored scan", {
  a <- association_for_fit(db, 1)
  expect_named(a, c("component", "field", "test", "statistic", "p_value", "padj"))
  expect_setequal(a$component, c("F1", "F2", "F3"))
  expect_setequal(a$test, c("spearman", "kruskal"))
  first_two <- association_for_fit(db, 1, 2)
  expect_setequal(first_two$component, c("F1", "F2"))
  # the adjustment is redone over the factors that are shown
  expect_equal(first_two$padj, p.adjust(first_two$p_value, "BH"))
  all_three <- association_for_fit(db, 1, 3)
  expect_equal(all_three$padj, a$padj)
  expect_equal(nrow(association_for_fit(db, 4)), 0)
})
