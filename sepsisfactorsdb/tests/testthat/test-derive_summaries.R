test_that("fit_summary ports the per-method definitions of the app's curve", {
  toy <- local_toy_db()
  fits <- toy_fits(c("pca", "nmf", "spca", "wgcna", "wgcna", "ica", "nmf"), mse = 0.5)
  fits$mse[1] <- NA
  fits$representative[7] <- 0L
  ctx <- toy_ctx(toy, fits)
  con <- toy$con
  DBI::dbAppendTable(con, "matrix_diagnostics", data.frame(dataset_id = "D", val_sd = 2))
  DBI::dbAppendTable(
    con,
    "fit_diag",
    data.frame(fit_id = 1L, name = "mse_by_rank", idx = 1:3, value = c(1, 0.6, 0.3))
  )
  DBI::dbAppendTable(con, "fit_metrics", data.frame(fit_id = 3L, name = "pev_sparse", value = 0.42))
  DBI::dbAppendTable(
    con,
    "module_kme_summary",
    data.frame(
      fit_id = 4L, module = 0:2, grp = "member", n = c(100, 10, 30), mean = c(0.1, 0.5, 0.7)
    )
  )
  DBI::dbAppendTable(
    con,
    "fit_pairs",
    data.frame(fit_a = c(4L, 4L), fit_b = c(5L, 6L), same_rank = 1L, ari = c(0.8, 0.6))
  )
  DBI::dbAppendTable(
    con,
    "factors",
    data.frame(
      fit_id = 6L, factor_index = 1:2, stability_cosine = 0.9, stability_pearson = 0.88,
      stability_spearman = 0.85
    )
  )
  expect_equal(derive_fit_summary(ctx), 6L)
  s <- DBI::dbGetQuery(con, "SELECT * FROM fit_summary ORDER BY fit_id")
  row <- function(i) s[s$fit_id == i, ]
  # val_sd is 2, so 1 - mse / 4
  expect_equal(row(2)$in_sample_r2, 1 - 0.5 / 4)
  expect_equal(row(1)$in_sample_r2, 1 - 0.6 / 4)
  expect_equal(row(1)$stability_cosine, 1)
  expect_equal(row(6)$in_sample_r2, 1 - 0.5 / 4)
  expect_equal(row(6)$stability_cosine, 0.9)
  expect_equal(row(6)$stability_spearman, 0.85)
  expect_true(is.na(row(2)$stability_cosine))
  # sPCA quality is its sparse PEV
  expect_equal(row(3)$in_sample_r2, 0.42)
  # WGCNA: the member-weighted mean kME of the modules but the grey one; stability is the mean ARI
  expect_equal(row(4)$in_sample_r2, (10 * 0.5 + 30 * 0.7) / 40)
  expect_equal(row(4)$stability_cosine, 0.7)
  expect_equal(row(5)$stability_spearman, 0.8)
  # only representative fits are summarised
  expect_false(7 %in% s$fit_id)
  # a chunk (a part of its own in a parallel build) summarises only its fits
  ctx$fits <- fits[2, ]
  expect_equal(derive_fit_summary(ctx), 1L)
  expect_equal(db_query(toy$db, "SELECT fit_id FROM fit_summary")$fit_id, 2L)
})

test_that("a fit summary without val_sd or a PCA rank has no in-sample r squared", {
  toy <- local_toy_db()
  ctx <- toy_ctx(toy, toy_fits(c("pca", "nmf"), mse = 0.5, n_factors = 5L))
  DBI::dbAppendTable(
    toy$con,
    "fit_diag",
    data.frame(fit_id = 1L, name = "mse_by_rank", idx = 1:3, value = c(1, 0.6, 0.3))
  )
  derive_fit_summary(ctx)
  s <- DBI::dbGetQuery(toy$con, "SELECT * FROM fit_summary ORDER BY fit_id")
  expect_equal(nrow(s), 2)
  expect_true(all(is.na(s$in_sample_r2)))
})

test_that("factor_association matches the scan of the scores and sample metadata", {
  withr::local_seed(2)
  toy <- local_toy_db()
  ids <- paste0("S", 1:10)
  meta <- data.frame(
    sample_id = ids, age = stats::rnorm(10), group = rep(c("a", "b"), 5), stringsAsFactors = FALSE
  )
  S <- matrix(stats::rnorm(30), 10, 3, dimnames = list(ids, NULL))
  save_artifact(toy, meta, "meta.rds")
  save_artifact(toy, S, "scores.rds")
  fit_files <- data.frame(fit_id = 1L, kind = "scores", file = "scores.rds")
  ctx <- toy_ctx(toy, toy_fits("ica", n_factors = 2L), fit_files, c(sample_metadata = "meta.rds"))
  DBI::dbAppendTable(
    toy$con,
    "factors",
    data.frame(fit_id = 1L, factor_index = 1:2, label = c("IC1", "IC2"))
  )
  expect_equal(derive_factor_association(ctx), 4L)
  a <- DBI::dbGetQuery(toy$con, "SELECT * FROM factor_association ORDER BY factor_index, field")
  expect_equal(a$test[a$field == "group"], c("kruskal", "kruskal"))
  rho <- stats::cor(S[, 1], meta$age, method = "spearman")
  expect_equal(a$statistic[a$field == "age" & a$factor_index == 1], rho)
  kt <- stats::kruskal.test(S[, 2] ~ factor(meta$group))
  expect_equal(a$p_value[a$field == "group" & a$factor_index == 2], kt$p.value)
  # only the first two scores columns are factors of the fit
  expect_equal(sort(unique(a$factor_index)), 1:2)
  # a dataset without sample metadata has no associations
  ctx$dataset_files <- character()
  expect_equal(derive_factor_association(ctx), 0L)
  expect_equal(db_count(toy$db, "factor_association"), 0)
})

test_that("p_adj is Benjamini-Hochberg over the grid of the scan", {
  withr::local_seed(3)
  ids <- paste0("S", 1:12)
  meta <- data.frame(sample_id = ids, age = stats::rnorm(12), group = rep(c("a", "b", "c"), 4))
  S <- matrix(stats::rnorm(36), 12, 3, dimnames = list(ids, c("F1", "F2", "F3")))
  a <- association_scan(S, meta)
  expect_equal(nrow(a), 6)
  expect_equal(a$padj, stats::p.adjust(a$p_value, "BH"))
  expect_setequal(a$test, c("spearman", "kruskal"))
})

test_that("the association scan skips fields without enough usable values", {
  S <- matrix(stats::rnorm(8), 4, 2, dimnames = list(letters[1:4], c("F1", "F2")))
  meta <- data.frame(sample_id = letters[1:4], const = 1, few = c(1, NA, NA, 2))
  expect_equal(nrow(association_scan(S, meta)), 0)
  expect_equal(nrow(association_scan(S[1:2, ], meta[1:2, ])), 0)
})
