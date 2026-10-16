test_that("fit_summary ports the per-method definitions of the app's curve", {
  x <- local_ingested()
  s <- db_query(x$db, "SELECT f.dataset_id, f.method, s.* FROM fit_summary s JOIN fits f USING (fit_id)")
  toy1 <- s[s$dataset_id == "TOY1", ]
  expect_setequal(toy1$method, c("pca", "spca", "ica", "nmf", "cogaps", "wgcna"))
  row <- function(m) toy1[toy1$method == m, ]
  # val_sd is 2 in the example, so 1 - mse / 4
  expect_equal(row("nmf")$in_sample_r2, 1 - 0.5 / 4)
  expect_equal(row("pca")$in_sample_r2, 1 - 0.3 / 4)
  expect_equal(row("pca")$stability_cosine, 1)
  expect_equal(row("ica")$stability_cosine, 0.9)
  expect_equal(row("ica")$stability_spearman, 0.85)
  expect_true(is.na(row("nmf")$stability_cosine))
  expect_equal(row("wgcna")$in_sample_r2, 0.5)
  expect_true(is.na(row("spca")$in_sample_r2))
  # only representative fits are summarised
  expect_equal(nrow(s), db_query(x$db, "SELECT COUNT(*) AS n FROM fits WHERE representative = 1 AND status = 'ok'")$n)
})

test_that("sPCA quality is its sparse PEV and WGCNA stability the mean ARI", {
  root <- local_results()
  keys <- function(m) read_table(root, "TOY1", "fits")$fit_key[read_table(root, "TOY1", "fits")$method == m]
  spca <- keys("spca")[1]
  wgcna <- keys("wgcna")
  edit_table(root, "TOY1", "fit_metrics", function(df) {
    rbind(df, data.frame(fit_key = spca, name = "pev_sparse", value = 0.42))
  })
  edit_table(root, "TOY1", "fit_pairs", function(df) {
    df$fit_a_key[2] <- wgcna[1]
    df$fit_b_key[2] <- wgcna[2]
    df
  })
  edit_table(root, "TOY1", "fits", function(df) {
    df$representative[df$fit_key == wgcna[2]] <- 1L
    df
  })
  db <- local_db()
  ingest_results(root, db)
  s <- db_query(db, "SELECT f.fit_key, s.* FROM fit_summary s JOIN fits f USING (fit_id)")
  expect_equal(s$in_sample_r2[s$fit_key == spca], 0.42)
  expect_equal(s$stability_cosine[s$fit_key == wgcna[1]], 0.8)
  expect_equal(s$stability_spearman[s$fit_key == wgcna[2]], 0.8)
})

test_that("factor_association matches the scan of the scores and sample metadata", {
  x <- local_ingested()
  key <- fit_key_of(x$db, "ica", "AND representative = 1")
  S <- read_artifact(x$root, "TOY1", paste0(key, "_scores.rds"))
  meta <- read_artifact(x$root, "TOY1", "sample_metadata.rds")
  a <- db_query(x$db, sprintf(
    "SELECT a.* FROM factor_association a JOIN fits f USING (fit_id) WHERE f.fit_key = '%s' ORDER BY factor_index, field", key
  ))
  expect_setequal(a$field, c("subject", "age", "group"))
  expect_equal(a$test[a$field == "group"][1], "kruskal")
  expect_equal(a$test[a$field == "age"][1], "spearman")
  rho <- stats::cor(S[meta$sample_id, 1], meta$age, method = "spearman")
  expect_equal(a$statistic[a$field == "age" & a$factor_index == 1], rho)
  kt <- stats::kruskal.test(S[meta$sample_id, 2] ~ factor(meta$group))
  expect_equal(a$p_value[a$field == "group" & a$factor_index == 2], kt$p.value)
})

test_that("p_adj is Benjamini-Hochberg over the fit's grid", {
  x <- local_ingested()
  a <- db_query(x$db, "SELECT fit_id, p_value, p_adj FROM factor_association")
  for (f in unique(a$fit_id)) {
    d <- a[a$fit_id == f, ]
    expect_equal(d$p_adj, stats::p.adjust(d$p_value, "BH"))
  }
})

test_that("the association scan skips fields without enough usable values", {
  S <- matrix(stats::rnorm(8), 4, 2, dimnames = list(letters[1:4], c("F1", "F2")))
  meta <- data.frame(sample_id = letters[1:4], const = 1, few = c(1, NA, NA, 2))
  expect_equal(nrow(association_scan(S, meta)), 0)
  expect_equal(nrow(association_scan(S[1:2, ], meta[1:2, ])), 0)
})

test_that("a dataset without sample metadata has no associations or metadata rows", {
  root <- local_results()
  edit_table(root, "TOY2", "dataset_artifacts", function(df) df[df$kind != "sample_metadata", ])
  db <- local_db()
  ingest_results(root, db)
  expect_equal(db_query(db, "SELECT COUNT(*) AS n FROM sample_metadata WHERE dataset_id = 'TOY2'")$n, 0)
  expect_equal(
    db_query(db, "SELECT COUNT(*) AS n FROM factor_association a JOIN fits f USING (fit_id) WHERE f.dataset_id = 'TOY2'")$n,
    0
  )
})
