db <- open_fixture()
info <- methods_table(db)

test_that("the cross-method module reads the stored agreement of the optimal fits", {
  optimal <- data.frame(
    method = c("pca", "spca"), fit_id = c(1, 2), n_factors = c(3, 2)
  )
  state <- list(
    dataset = reactive("TOY1"), optimal = reactive(optimal), info = info
  )
  testServer(mod_cross_method_server, args = list(state = state, db = db), {
    session$setInputs(top_n = "25", view = "gene_jaccard")
    a <- agree()
    expect_equal(a$methods, c("pca", "spca"))
    expect_false(is.na(a$gene_jaccard["pca", "spca"]))
    expect_named(a$pairs, "pca|spca")
    session$setInputs(top_n = "10")
    expect_equal(dim(agree()$gene_jaccard), c(2, 2))
  })
})

test_that("facets say when the hyperparameter is a power", {
  expect_equal(
    strip_label(info, c("pca", "wgcna"), c("rank 3", "power 8")),
    c("PCA", "WGCNA (x = power)")
  )
})
