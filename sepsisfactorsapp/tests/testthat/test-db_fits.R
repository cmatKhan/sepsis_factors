db <- open_fixture()
info <- methods_table(db)

test_that("the metric must be one the database has", {
  expect_snapshot(check_metric("euclid"), error = TRUE)
  expect_equal(check_metric("jaccard", MATCH_METRICS), "jaccard")
  expect_error(check_metric("jaccard"), "must be one of")
})

test_that("PCA's curve is 1 - mse / val_sd^2 per rank, with stability 1", {
  cv <- method_curve(db, "TOY1", "pca")
  expect_equal(cv$rank_key, 1:3)
  expect_equal(cv$rank_label, paste("rank", 1:3))
  expect_equal(cv$in_sample_r2, 1 - c(1, 0.6, 0.3) / 4)
  expect_equal(cv$stability, c(1, 1, 1))
  expect_equal(unique(cv$fit_id), 1)
})

test_that("other curves come from fit_summary, by rank or by power", {
  cv <- method_curve(db, "TOY1", "ica", "pearson")
  expect_equal(nrow(cv), 1)
  expect_equal(cv$in_sample_r2, 0.875)
  expect_equal(cv$stability, 0.88)
  expect_equal(method_curve(db, "TOY1", "ica", "spearman")$stability, 0.85)
  # the toy WGCNA fits carry no power, so give them one
  copy <- fixture_copy()
  con <- DBI::dbConnect(RSQLite::SQLite(), copy$path)
  DBI::dbExecute(con, "INSERT INTO fit_params (fit_id, name, num_value) VALUES (12, 'power', 8)")
  DBI::dbDisconnect(con)
  powered <- open_db(copy$path)
  withr::defer(close_db(powered))
  w <- method_curve(powered, "TOY1", "wgcna")
  expect_equal(w$rank_label, "power 8")
  expect_equal(w$rank_key, 8)
  expect_true(is.na(w$stability))
  expect_equal(nrow(method_curve(db, "TOY2", "nmf")), 0)
  expect_error(method_curve(db, "TOY1", "ica", "jaccard"), "must be one of")
})

test_that("curves stack across methods", {
  cv <- dataset_curves(db, "TOY1", info$method)
  expect_setequal(cv$method, info$method)
  expect_named(cv, names(empty_curve()))
})

test_that("the optimal fit follows the rule and comes out in display order", {
  curve <- data.frame(
    method = "x", rank_label = paste("rank", 1:4), rank_key = 1:4, fit_id = 1:4,
    n_factors = 1:4, in_sample_r2 = c(0.2, 0.5, 0.7, 0.8),
    stability = c(0.99, 0.95, 0.92, 0.5)
  )
  expect_equal(pick_optimal(curve, "stable", 0.9)$rank_key, 3)
  expect_equal(pick_optimal(curve, "stable", 0.96)$rank_key, 1)
  expect_equal(pick_optimal(curve, "stable", 0.999)$rank_key, 1)
  expect_equal(pick_optimal(curve, "max_r2")$rank_key, 4)
  expect_null(pick_optimal(curve[0, ]))
  curve$stability <- NA
  expect_equal(pick_optimal(curve, "stable")$rank_key, 4)

  cv <- dataset_curves(db, "TOY1", rev(info$method))
  # the toy sPCA fit has no quality, so it has no optimal fit
  expect_equal(optimal_fits(cv, info)$method, setdiff(info$method, "spca"))
  expect_equal(nrow(optimal_fits(empty_curve(), info)), 0)
})

test_that("out-of-sample scores use one projection variant per type", {
  oos <- oos_r2_by_method(db, "TOY1")
  expect_equal(oos$method, "pca")
  expect_true(is.na(oos$oos_r2_within))
  expect_equal(oos$oos_r2_cross, 0.8)
  expect_equal(nrow(oos_r2_by_method(db, "TOY2")), 0)

  copy <- fixture_copy()
  con <- DBI::dbConnect(RSQLite::SQLite(), copy$path)
  DBI::dbExecute(
    con,
    "INSERT INTO projections (source_fit_id, target_dataset_id, projection_type,
       include_intercept, relative_r2, path) VALUES (1, 'TOY2', 'cross_dataset', 0, 0.1, 'x')"
  )
  DBI::dbDisconnect(con)
  mixed <- open_db(copy$path)
  withr::defer(close_db(mixed))
  expect_equal(oos_r2_by_method(mixed, "TOY1")$oos_r2_cross, 0.8)
  expect_equal(cross_dataset_similarity(mixed, "pca")$value, 0.8)
  expect_equal(nrow(projections_for_pair(mixed, "TOY1", "TOY2")), 1)
  expect_equal(projection_targets(mixed, "TOY1"), "TOY2")
})

test_that("cross-dataset similarity is long, then a clustered square matrix", {
  long <- cross_dataset_similarity(db, "pca")
  expect_equal(long$source, "TOY1")
  expect_equal(long$target, "TOY2")
  expect_equal(long$n, 1)
  expect_equal(nrow(cross_dataset_similarity(db, "nmf")), 0)
  gs <- cross_dataset_similarity(db, "pca", "gene_space")
  expect_equal(gs$value, 0.7)
  m <- similarity_matrix(long)
  expect_equal(dim(m), c(2, 2))
  expect_equal(m["TOY1", "TOY2"], 0.8)
  expect_true(is.na(m["TOY1", "TOY1"]))
  expect_null(similarity_matrix(empty_similarity()))
})

test_that("same-rank matches and their null come with the chosen metric", {
  d <- same_rank_matches(db, "TOY1", "ica", "cosine")
  expect_named(d, c("rank", "fit_pair_id", "value", "kind"))
  expect_gt(nrow(d), 0)
  expect_true(all(d$value >= 0))
  expect_equal(nrow(same_rank_matches(db, "TOY1", "ica", "jaccard")), 0)
  expect_named(same_rank_null(db, "TOY1", "ica", "cosine"), c("rank", "p95"))
  m <- seed_pair_matrix(db, "TOY1", "ica", 2, "cosine")
  expect_equal(dim(m), rep(length(unique(rownames(m))), 2))
  expect_equal(m, t(m))
})

test_that("per-factor stability takes the chosen metric", {
  d <- fit_factor_stability(db, 5, "pearson")
  expect_named(d, c("factor_index", "label", "stability", "n_genes"))
  expect_equal(d$stability, fit_factors(db, 5)$stability_pearson)
})

test_that("PCA's scree gives error and cumulative variance by rank", {
  d <- pca_scree(db, "TOY1")
  expect_equal(d$rank, 1:3)
  expect_equal(d$mse, c(1, 0.6, 0.3))
  expect_equal(d$pev, cumsum(c(2, 1, 0.5)^2) / sum(c(2, 1, 0.5)^2))
  expect_null(pca_scree(db, "NOPE"))
})

test_that("factors can be cut to the first n", {
  expect_equal(nrow(fit_factors(db, 1)), 3)
  expect_equal(fit_factors(db, 1, 2)$factor_index, 1:2)
})

test_that("method-specific tables read without error", {
  expect_gt(nrow(method_fits(db, "TOY1", "spca")), 0)
  expect_true(is.null(spca_is_table(db, "TOY1")) || is.data.frame(spca_is_table(db, "TOY1")))
  expect_s3_class(icasso_iq(db, "TOY1"), "data.frame")
  expect_true(is.matrix(wgcna_ari_matrix(db, "TOY1")) || is.null(wgcna_ari_matrix(db, "TOY1")))
  expect_s3_class(wgcna_power_summary(db, "TOY1"), "data.frame")
  expect_s3_class(wgcna_gene_modules(db, 12), "data.frame")
  expect_s3_class(wgcna_kme_summary(db, 12), "data.frame")
  expect_s3_class(enrichment_queries_for_fit(db, 1), "data.frame")
  expect_s3_class(factor_matches_everywhere(db, 1, "jaccard"), "data.frame")
  expect_error(factor_matches_everywhere(db, 1, "euclid"), "must be one of")
  expect_s3_class(gene_significance_fields(db, "TOY1"), "data.frame")
})

test_that("module overlap is the Jaccard index of the modules' genes", {
  expect_equal(jaccard(c("a", "b"), c("b", "c")), 1 / 3)
  expect_true(is.na(jaccard(character(0), character(0))))
  m <- jaccard_matrix(list(x = c("a", "b"), y = "c"), list(p = c("b", "c")))
  expect_equal(unname(m[, "p"]), c(1 / 3, 1 / 2))
})
