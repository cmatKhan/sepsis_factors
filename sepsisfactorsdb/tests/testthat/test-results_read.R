test_that("the example results validate", {
  root <- local_results()
  manifest <- validate_results(root)
  expect_equal(manifest$datasets, c("TOY1", "TOY2"))
  expect_equal(validate_results(root, "TOY2")$datasets, c("TOY1", "TOY2"))
})

test_that("a directory without a manifest is not a results output", {
  dir <- withr::local_tempdir()
  expect_snapshot(validate_results(dir), error = TRUE, transform = scrub_tmp)
})

test_that("another results-output version is refused", {
  root <- local_results()
  edit_manifest(root, function(m) {
    m$results_output_version <- 3L
    m
  })
  expect_snapshot(validate_results(root), error = TRUE, transform = scrub_tmp)
  expect_error(validate_results(root), class = "sepsisfactorsdb_results_version")
})

test_that("an unknown method is refused with a pointer to the issue tracker", {
  root <- local_results()
  edit_manifest(root, function(m) {
    m$methods$umap <- list(method_output_version = 1L)
    m
  })
  expect_snapshot(validate_results(root), error = TRUE, transform = scrub_tmp)
  expect_error(validate_results(root), class = "sepsisfactorsdb_unsupported_method")
})

test_that("an unsupported method output version is refused", {
  root <- local_results()
  edit_manifest(root, function(m) {
    m$methods$pca$method_output_version <- 2L
    m
  })
  expect_snapshot(validate_results(root), error = TRUE, transform = scrub_tmp)
})

test_that("a dataset missing from the manifest cannot be selected", {
  root <- local_results()
  expect_snapshot(validate_results(root, "NOPE"), error = TRUE, transform = scrub_tmp)
})

test_that("missing and unexpected columns are reported", {
  root <- local_results()
  edit_table(root, "TOY1", "fit_params", function(df) df[, c("fit_key", "name", "num_value")])
  expect_snapshot(validate_results(root), error = TRUE, transform = scrub_tmp)

  root <- local_results()
  edit_table(root, "TOY1", "fit_params", function(df) cbind(df, extra = 1L))
  expect_snapshot(validate_results(root), error = TRUE, transform = scrub_tmp)
})

test_that("a table that is not in the contract is refused", {
  root <- local_results()
  arrow::write_parquet(
    data.frame(x = 1),
    file.path(root, "datasets", "TOY1", "tables", "stray.parquet")
  )
  expect_error(validate_results(root), class = "sepsisfactorsdb_invalid_results")
})

test_that("a table holding another dataset's rows is refused", {
  root <- local_results()
  edit_table(root, "TOY1", "samples", function(df) {
    df$dataset_id <- "TOY2"
    df
  })
  expect_snapshot(validate_results(root), error = TRUE, transform = scrub_tmp)
})

test_that("keys must refer to fits that exist", {
  root <- local_results()
  edit_table(root, "TOY1", "fit_params", function(df) {
    df$fit_key[1] <- "pca_ffffffffffffffff"
    df
  })
  expect_snapshot(validate_results(root), error = TRUE, transform = scrub_tmp)
})

test_that("a missing artifact file is reported", {
  root <- local_results()
  files <- list.files(file.path(root, "datasets", "TOY1", "artifacts"), pattern = "_loadings\\.rds$", full.names = TRUE)
  unlink(files[1])
  expect_snapshot(validate_results(root), error = TRUE, transform = scrub_tmp)
})

test_that("an ok fit without the artifacts its method requires is refused", {
  root <- local_results()
  edit_table(root, "TOY1", "fit_artifacts", function(df) df[!(df$kind == "diag" & grepl("^nmf_", df$fit_key)), ])
  expect_snapshot(validate_results(root), error = TRUE, transform = scrub_tmp)
})

test_that("bootstrap ICA fits need no scores but other ICA fits do", {
  root <- local_results()
  edit_table(root, "TOY1", "fit_artifacts", function(df) {
    df[!(df$kind == "scores" & grepl("^ica_", df$fit_key))[TRUE], ]
  })
  expect_error(validate_results(root), class = "sepsisfactorsdb_invalid_results")
})

test_that("a methods row that disagrees with the method's output spec is refused", {
  root <- local_results()
  edit_table(root, "TOY1", "methods", function(df) {
    df$has_loadings[df$method == "wgcna"] <- 1L
    df
  })
  expect_snapshot(validate_results(root), error = TRUE, transform = scrub_tmp)
})

test_that("a method that requires a table is refused without it", {
  root <- local_results()
  unlink(file.path(root, "datasets", "TOY1", "tables", "gene_modules.parquet"))
  expect_snapshot(validate_results(root), error = TRUE, transform = scrub_tmp)
})
