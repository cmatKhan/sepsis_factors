test_that("the workflow runs end to end on synthetic datasets and writes a loadable results output", {
  skip_on_cran()
  skip_if_not_installed("WGCNA")
  skip_if_not_installed("NNLM")
  skip_if_not_installed("fastICA")
  skip_if_not_installed("elasticnet")
  skip_if_not_installed("projectR")
  skip_if_not_installed("variancePartition")
  p <- local_project(design = list(SYN1 = "design: {platform: array, group: group}"))
  make_project_targets(p$project)

  results <- file.path(p$project, "results")
  manifest <- jsonlite::read_json(file.path(results, "manifest.json"), simplifyVector = TRUE)
  expect_equal(manifest$results_output_version, RESULTS_OUTPUT_VERSION)
  expect_setequal(manifest$datasets, p$ids)
  expect_setequal(names(manifest$methods), c("pca", "spca", "ica", "nmf", "wgcna"))
  for (id in p$ids) {
    tables <- list.files(file.path(results, "datasets", id, "tables"))
    expect_contains(tables, c("datasets.parquet", "methods.parquet", "fits.parquet", "factors.parquet"))
  }
  expect_contains(
    list.files(file.path(results, "datasets", "SYN1", "tables")),
    c("dataset_design.parquet", "samples.parquet", "variance_summary.parquet", "factor_variance.parquet")
  )
  expect_contains(list.files(file.path(results, "cross_dataset", "tables")), "projections.parquet")

  # No method fit failed on the synthetic data.
  fits <- as.data.frame(arrow::read_parquet(file.path(results, "datasets", "SYN1", "tables", "fits.parquet")))
  expect_equal(fits$error[fits$status == "failed"], character(0))

  # sepsisfactorsdb (a sibling checkout) accepts it.
  db_pkg <- normalizePath(testthat::test_path("..", "..", "..", "sepsisfactorsdb"), mustWork = FALSE)
  skip_if_not(dir.exists(db_pkg), "sepsisfactorsdb is not next to this package")
  counts <- callr::r(
    function(results, db_pkg) {
      suppressMessages(pkgload::load_all(db_pkg, quiet = TRUE))
      validate_results(results)
      db <- tempfile(fileext = ".sqlite")
      suppressMessages(ingest_results(results, db))
      con <- DBI::dbConnect(RSQLite::SQLite(), db)
      on.exit(DBI::dbDisconnect(con))
      n <- function(t) DBI::dbGetQuery(con, paste("SELECT COUNT(*) AS n FROM", t))$n
      list(
        fits = n("fits"), factors = n("factors"), projections = n("projections"),
        gene_modules = n("gene_modules"), variance_summary = n("variance_summary"),
        factor_variance = n("factor_variance"), enrichment_results = n("enrichment_results"),
        foreign_key_violations = nrow(DBI::dbGetQuery(con, "PRAGMA foreign_key_check"))
      )
    },
    args = list(results = results, db_pkg = db_pkg),
    libpath = .libPaths()
  )
  expect_gt(counts$fits, 0)
  expect_gt(counts$factors, 0)
  expect_gt(counts$projections, 0)
  expect_gt(counts$gene_modules, 0)
  expect_equal(counts$variance_summary, 1)
  expect_gt(counts$factor_variance, 0)
  expect_gt(counts$enrichment_results, 0)
  expect_equal(counts$foreign_key_violations, 0)
})
