test_that("write_tables skips empty tables and turns factors into character", {
  dir <- withr::local_tempdir()
  paths <- write_tables(
    list(a = data.frame(x = factor(c("u", "v")), y = 1:2), b = NULL, c = data.frame(x = integer())),
    dir
  )
  expect_equal(basename(paths), "a.parquet")
  expect_equal(list.files(dir), "a.parquet")
  expect_equal(arrow::read_parquet(paths)$x, c("u", "v"))
})

test_that("rbind_parts binds a component across methods and skips missing ones", {
  methods <- list(a = list(v = data.frame(k = 1)), b = list(v = NULL), c = list(v = data.frame(k = 2)))
  expect_equal(rbind_parts(methods, function(p, m) p$v)$k, c(1, 2))
  expect_null(rbind_parts(list(a = list()), function(p, m) p$v))
})

test_that("the cross-dataset tables join projection scores and drop stale projection files", {
  root <- withr::local_tempdir()
  dir.create(cross_artifacts_dir(root), recursive = TRUE)
  file.create(file.path(cross_artifacts_dir(root), c("keep.rds", "stale.rds")))
  projections <- list(data.frame(
    source_fit_key = "pca_1", target_dataset_id = "T", projection_type = "cross_dataset",
    include_intercept = 1L, n_genes_matched = 5L, n_samples = 3L, mean_pval = 0.1, median_pval = 0.1,
    mean_pvar = 0.2, median_pvar = 0.2, path = "keep.rds"
  ))
  scores <- list(data.frame(
    source_fit_key = "pca_1", target_dataset_id = "T", n_components = 2L, transfer_r2 = 0.5,
    native_r2 = 0.6, relative_r2 = 0.8, null_r2 = 0.01
  ))
  paths <- write_cross_results(root, NULL, projections, list(), NULL, scores)
  expect_equal(basename(paths), "projections.parquet")
  tab <- as.data.frame(arrow::read_parquet(paths))
  expect_equal(tab$relative_r2, 0.8)
  expect_equal(list.files(cross_artifacts_dir(root)), "keep.rds")
})

test_that("the manifest lists datasets with tables and the output version of the methods present", {
  root <- withr::local_tempdir()
  write_tables(list(datasets = data.frame(dataset_id = "D"), methods = data.frame(method = c("pca", "ica"))),
               tables_dir(root, "D"))
  path <- write_results_manifest(root, c("D", "MISSING"))
  manifest <- jsonlite::read_json(path, simplifyVector = TRUE)
  expect_equal(manifest$datasets, "D")
  expect_equal(manifest$results_output_version, RESULTS_OUTPUT_VERSION)
  expect_equal(manifest$methods$pca$method_output_version, 1)
  expect_equal(manifest$writer$package, "sepsisfactors")
})

test_that("optimal fits follow each method's rule", {
  ingest <- list(
    method = "nmf",
    fits = data.frame(
      fit_key = c("a", "b", "c", "d"), rank = c(2, 2, 3, 3), mse = c(0.5, 0.4, 0.2, 0.3),
      status = "ok", bootstrap = 0L
    ),
    metrics = data.frame(fit_key = character(), name = character(), value = numeric())
  )
  expect_equal(optimal_fit_key(ingest, character()), "c")
  ingest$method <- "pca"
  expect_equal(optimal_fit_key(ingest, character()), "a")
  ingest$method <- "wgcna"
  expect_null(optimal_fit_key(ingest, character()))
  ingest$method <- "spca"
  ingest$metrics <- data.frame(fit_key = c("a", "b", "c"), name = "pev_sparse", value = c(0.1, 0.9, 0.5))
  expect_equal(optimal_fit_key(ingest, c("a", "c")), "c")
})

test_that("representative fits follow each method's rule and never include bootstrap fits", {
  fits <- data.frame(
    fit_key = c("a", "b", "c", "d", "e"), rank = c(2, 2, 3, 3, 3), mse = c(0.5, 0.4, 0.3, 0.2, 0.1),
    status = c("ok", "ok", "ok", "ok", "ok"), bootstrap = c(0L, 0L, 0L, 0L, 1L)
  )
  ingest <- list(fits = fits)
  expect_setequal(representative_keys(ingest, method_spec("nmf")), c("b", "d"))
  expect_equal(representative_keys(ingest, method_spec("pca")), "c")
  expect_setequal(representative_keys(ingest, method_spec("wgcna")), c("a", "b", "c", "d"))
  selection <- data.frame(fit_key = c("a", "d"), selected = c(TRUE, TRUE))
  expect_setequal(representative_keys(ingest, method_spec("spca"), selection), c("a", "d"))
})
