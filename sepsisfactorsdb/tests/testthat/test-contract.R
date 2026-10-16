test_that("contract columns replace surrogate ids with natural keys", {
  expect_equal(contract_columns("fit_params"), c("fit_key", "name", "num_value", "text_value"))
  expect_false("fit_id" %in% contract_columns("factors"))
  expect_equal(contract_columns("factors")[1], "fit_key")
  expect_equal(
    contract_columns("factor_matches"),
    c("fit_a_key", "fit_b_key", "factor_a_index", "factor_b_index")
  )
})

test_that("loader-filled timestamps are optional", {
  expect_false("written_at" %in% contract_columns("fits"))
  expect_true("written_at" %in% contract_columns("fits", include_optional = TRUE))
})

test_that("every results table is a schema table with columns", {
  tabs <- results_tables()
  expect_true(all(tabs$table %in% names(schema_columns())))
  for (table in tabs$table) expect_gt(length(contract_columns(table)), 0)
})

test_that("an unknown table is an error", {
  expect_snapshot(contract_columns("nope"), error = TRUE)
})

test_that("supported methods are the closed set", {
  expect_equal(sort(supported_methods()$method), c("cogaps", "ica", "nmf", "pca", "spca", "wgcna"))
})
