test_that("contract columns replace surrogate ids with natural keys", {
  expect_equal(contract_columns("fit_params"), c("fit_key", "name", "num_value", "text_value"))
  expect_false("fit_id" %in% contract_columns("factors"))
  expect_equal(contract_columns("factors")[1], "fit_key")
  expect_equal(
    contract_columns("factor_matches"),
    c("fit_a_key", "fit_b_key", "factor_a_index", "factor_b_index")
  )
})

test_that("columns the database does not store and optional columns are not required", {
  expect_false("written_at" %in% contract_columns("fits"))
  expect_true("written_at" %in% contract_columns("fits", include_optional = TRUE))
  expect_false("family" %in% contract_columns("datasets"))
  expect_contains(
    contract_columns("datasets", include_optional = TRUE),
    c("family", "family_position", "hf_collection")
  )
  expect_false("label" %in% contract_columns("methods", include_optional = TRUE))
})

test_that("every loaded results table is a schema table, the others list their columns", {
  tabs <- results_tables()
  expect_true(all(tabs$table[tabs$loaded] %in% names(schema_columns())))
  expect_false(any(tabs$table[!tabs$loaded] %in% names(schema_columns())))
  for (table in tabs$table) expect_gt(length(contract_columns(table)), 0)
})

test_that("the derived tables are not part of the contract", {
  derived <- c(
    "genes", "factor_top_genes", "fit_scores", "projection_values", "driver_genes", "fit_diag",
    "wgcna_dendrograms", "sample_metadata", "gene_variance", "fit_summary", "method_agreement",
    "factor_association"
  )
  expect_length(intersect(derived, results_tables()$table), 0)
  expect_contains(names(schema_columns()), derived)
})

test_that("every method has display metadata for the methods table", {
  for (m in names(SUPPORTED_METHOD_OUTPUTS)) {
    d <- method_display(m)
    expect_true(all(nzchar(c(d$label, d$color, d$blurb))))
  }
  expect_equal(
    sort(vapply(names(SUPPORTED_METHOD_OUTPUTS), function(m) method_display(m)$order, 1L)),
    c(1:6),
    ignore_attr = TRUE
  )
})

test_that("an unknown table is an error", {
  expect_snapshot(contract_columns("nope"), error = TRUE)
})

test_that("supported methods are the closed set", {
  expect_equal(sort(supported_methods()$method), c("cogaps", "ica", "nmf", "pca", "spca", "wgcna"))
})
