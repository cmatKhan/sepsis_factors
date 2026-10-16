db <- open_fixture()

test_that("datasets and their family come from the database", {
  expect_equal(list_datasets(db), c("TOY1", "TOY2"))
  info <- datasets_info(db)
  expect_equal(info$family, c("TOY1", "TOY2"))
  expect_equal(info$subject_id_col, c("subject", "subject"))
  expect_equal(dataset_subject_col(db, "TOY1"), "subject")
  expect_true(is.na(dataset_subject_col(db, "NOPE")))
  expect_equal(family_positions(db, c("TOY2", "TOY1", "NOPE")), c(1, 1, NA))
})

test_that("methods are labelled, coloured and ordered from the methods table", {
  info <- methods_table(db)
  expect_equal(info$method, c("pca", "spca", "ica", "nmf", "cogaps", "wgcna"))
  expect_equal(method_label(info, c("wgcna", "pca", "zzz")), c("WGCNA", "PCA", "zzz"))
  expect_equal(method_colors(info)[["nmf"]], "#54A24B")
  expect_equal(order_methods(info, c("wgcna", "nmf", "pca", NA)), c("pca", "nmf", "wgcna"))
  expect_equal(order_methods(info, "zzz"), character(0))
  expect_equal(
    method_has_loadings(info, c("pca", "wgcna")), c(TRUE, FALSE)
  )
  expect_equal(methods_for_dataset(db, info, "TOY2"), "pca")
  expect_equal(methods_for_dataset(db, info, "TOY1"), info$method)
  cov <- fit_coverage(db, "TOY2")
  expect_equal(cov$method, "pca")
  expect_equal(cov$n_representative, 1)
})

test_that("timepoints follow the family position, else the number in the id", {
  expect_equal(order_timepoints(c("A_D1", "A_PRE", "A_D2"), c(2, 1, 3)), c(2, 1, 3))
  expect_equal(order_timepoints(c("A_D10", "A_D2", "A_PRE")), c(3, 2, 1))
  expect_equal(order_timepoints(c("A_D10", "A_D2", "A_PRE"), c(1, NA, 2)), c(3, 2, 1))
  expect_equal(order_timepoints(c("A_POST", "A_D1", "A_D2")), c(2, 3, 1))
})

test_that("the docs link follows the dataset's collection", {
  expect_null(dataset_docs_url(db, "TOY1"))
  info <- datasets_info(db)
  expect_equal(info$hf_collection, c("toy/collection", "toy/collection"))
  copy <- fixture_copy()
  con <- DBI::dbConnect(RSQLite::SQLite(), copy$path)
  DBI::dbExecute(
    con, "UPDATE datasets SET hf_collection = ? WHERE dataset_id = 'TOY1'",
    params = list(PUBLIC_COLLECTION)
  )
  DBI::dbDisconnect(con)
  copy2 <- open_db(copy$path)
  withr::defer(close_db(copy2))
  expect_equal(
    dataset_docs_url(copy2, "TOY1"), paste0(DOCS_URL, "datasets.html#toy1")
  )
  expect_equal(
    dataset_docs_url(copy2, "TOY1_T1", "TOY1"), paste0(DOCS_URL, "datasets.html#toy1")
  )
  expect_null(dataset_docs_url(copy2, "TOY2"))
})

test_that("composition markers are grouped by set", {
  m <- composition_markers(db)
  expect_equal(m$neutrophil, c("CXCR2", "FCGR3B"))
  expect_equal(m$lymphocyte, "CD3D")
})

test_that("genes are labelled by symbol, falling back to the id", {
  g <- dataset_genes(db, "TOY1")
  expect_equal(nrow(g), 10)
  labels <- gene_labels(db, "TOY1")
  expect_equal(labels[["ENSG00000000001"]], "SYM1")
  expect_equal(label_genes(labels, c("ENSG00000000002", "other")), c("SYM2", "other"))
})
