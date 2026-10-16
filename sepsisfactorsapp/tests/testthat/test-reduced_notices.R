db <- open_fixture()

test_that("the biplot says its gene arrows come from the stored genes, only then", {
  notice <- as.character(biplot_notice(load_loadings(db, 1)))
  expect_match(notice, "top 50 genes per side", fixed = TRUE)
  expect_match(notice, "stability_artifacts/", fixed = TRUE)
  expect_null(biplot_notice(with_source(matrix(0), "artifact")))
  expect_null(biplot_notice(NULL))
})

test_that("the pattern-driver page only reports the volcano background as reduced", {
  from_db <- with_source(data.frame(gene = "a"), "database")
  expect_match(as.character(driver_notice(from_db, "PV")), "significant", fixed = TRUE)
  expect_match(as.character(driver_notice(from_db, "PV")), "stability_artifacts/", fixed = TRUE)
  expect_null(driver_notice(from_db, "CI"))
  expect_null(driver_notice(with_source(data.frame(gene = "a"), "artifact"), "PV"))
  expect_null(driver_notice(NULL, "PV"))
})

test_that("the notices name the full output as the way to the complete data", {
  expect_match(LOADINGS_REDUCED, "top 50 genes")
  expect_match(DRIVERS_REDUCED, "non-significant")
  expect_match(REDUCED_HINT, "sepsisfactors output")
})
