test_that("model formulas split into random and fixed terms", {
  f <- formula_terms("~ 0 + cell + (1 | subject) + age")
  expect_equal(f$variable, c("cell", "subject", "age"))
  expect_equal(f$random, c(FALSE, TRUE, FALSE))
  expect_null(formula_terms(NA_character_))
})

test_that("cells count samples and subjects, keeping empty ones", {
  s <- data.frame(
    timepoint = c("T0", "T0", "T1"), grp = c("a", "a", NA), subject = c("x", "y", "x")
  )
  g <- cell_table(s, c("T0", "T1"))
  expect_equal(g[["T0"]], c("2 / 2", "-"))
  expect_equal(g[["T1"]], c("-", "1 / 1"))
  expect_equal(rownames(g), c("a", "all samples"))
  expect_null(cell_table(s[0, ]))
  expect_null(cell_table(transform(s, timepoint = NA)))
})

test_that("flags name the datasets whose sex or fit quality looks wrong", {
  s <- data.frame(
    sex_agreement = c(0.5, 1), share_singular = c(0, 0.95), n_genes_failed = c(0, 3)
  )
  expect_equal(
    dataset_flags(s), c("sex agreement 0.50", "95% singular fits; 3 genes failed")
  )
})

test_that("shares are summed into the headline parts", {
  long <- data.frame(
    dataset_id = "d", category = c("subject", "subject-level", "time", "residual"),
    share = c(0.2, 0.1, 0.3, 0.4)
  )
  s <- data.frame(
    dataset_id = "d", repeatability_median = 1, time_within_median = 2, share_time_gt10 = 3,
    n_subjects_repeated = 4, n_genes = 5, sex_agreement = NA, share_singular = NA,
    n_genes_failed = NA
  )
  h <- headline_table(long, s)
  expect_equal(h$between, 0.3)
  expect_equal(h$time, 0.3)
  expect_equal(h$residual, 0.4)
  expect_equal(h$technical, 0)
})

test_that("variance shares are read for the chosen gene set", {
  db <- open_fixture()
  d <- variance_long(db, "TOY1", "vw_all")
  expect_named(d, c("dataset_id", "category", "present", "share"))
  expect_s3_class(d$category, "factor")
  expect_error(variance_long(db, "TOY1", "nonsense"))
  expect_true(variance_ready(db))
  pc1 <- pc1_composition_table(db)
  expect_true(is.null(pc1) || is.data.frame(pc1))
  expect_lte(nrow(time_genes_for(db, "TOY1", 1)), 1)
  expect_equal(nrow(time_gene_recurrence(db, 2)), 2)
})
