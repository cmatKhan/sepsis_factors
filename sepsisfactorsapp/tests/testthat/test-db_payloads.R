db <- open_fixture()

# Where the database says fit 1's full artifact of one kind is.
artifact_pointer <- function(db, kind) {
  fit_pointer(db, 1, kind)
}

test_that("loadings from the database are a sparse view of the stored top genes", {
  L <- load_loadings(db, 1)
  expect_true(from_database(L))
  expect_equal(ncol(L), 3)
  expect_equal(L["ENSG00000000004", 1], 1.5952808, tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(L["ENSG00000000003", 1], -0.8356286, tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(ncol(load_loadings(db, 1, n = 2)), 2)
  # WGCNA has modules, not loadings
  expect_null(load_loadings(db, 12))
})

test_that("the full loadings win when the artifact file exists", {
  copy <- fixture_copy()
  path <- artifact_pointer(copy, "loadings")
  full <- matrix(
    seq_len(60) / 10, 20, 3,
    dimnames = list(sprintf("G%d", 1:20), NULL)
  )
  expect_true(from_database(load_loadings(copy, 1)))
  write_artifact(copy, path, full)
  L <- load_loadings(copy, 1)
  expect_false(from_database(L))
  expect_identical(attr(L, "source"), "artifact")
  expect_equal(unname(L), unname(full), ignore_attr = "source")
  expect_equal(dim(load_loadings(copy, 1, n = 2)), c(20, 2))
})

test_that("top genes of a loadings column come from both sides", {
  L <- matrix(
    c(3, 1, -2, 0.5, 0, -4), 6, 1,
    dimnames = list(letters[1:6], NULL)
  )
  d <- top_loading_genes(L, 1, n = 2)
  expect_equal(d$gene, c("a", "b", "f", "c"))
  expect_equal(d$side, c("positive", "positive", "negative", "negative"))
  nn <- top_loading_genes(abs(L), 1, n = 2)
  expect_equal(unique(nn$side), "positive")
})

test_that("sparse loadings put zero where a gene was not stored", {
  rows <- data.frame(
    factor_index = c(1, 1, 2), gene_id = c("a", "b", "b"), loading = c(1, -2, 3)
  )
  L <- sparse_loadings(rows)
  expect_equal(L, matrix(c(1, -2, 0, 3), 2, dimnames = list(c("a", "b"), NULL)))
  expect_equal(ncol(sparse_loadings(rows, 1)), 1)
  expect_null(sparse_loadings(rows[0, ]))
})

test_that("scores decode from the blob, named by the factor labels", {
  S <- load_scores(db, 1)
  expect_true(from_database(S))
  expect_equal(dim(S), c(6, 3))
  expect_equal(rownames(S), sprintf("TOY1_S%d", 1:6))
  expect_equal(colnames(S), c("F1", "F2", "F3"))
  expect_equal(colnames(load_scores(db, 1, n = 2)), c("F1", "F2"))
  row <- data.frame(row_ids = encode_ids(c("s1", "s2")), n_factors = 3L)
  row$data <- list(encode_f32(1:6))
  S <- decode_scores(row, c("A", "B"))
  expect_equal(S, matrix(as.numeric(1:6), 2, dimnames = list(c("s1", "s2"), c("A", "B", "extra1"))))
})

test_that("full scores are used when the artifact exists", {
  copy <- fixture_copy()
  path <- artifact_pointer(copy, "scores")
  full <- matrix(1:12 + 0.123456789, 4, 3, dimnames = list(paste0("s", 1:4), NULL))
  write_artifact(copy, path, full)
  S <- load_scores(copy, 1)
  expect_identical(attr(S, "source"), "artifact")
  expect_equal(unname(S), unname(full), ignore_attr = "source")
})

test_that("the focus fit's scores are named by factor label", {
  focus <- list(fit_id = 1, n_factors = 2, has_loadings = TRUE)
  expect_equal(colnames(focus_scores(db, focus)), c("F1", "F2"))
  module <- list(fit_id = 12, n_factors = 2, has_loadings = FALSE)
  S <- focus_scores(db, module)
  expect_true(all(colnames(S) %in% fit_factors(db, 12)$label))
  expect_null(focus_scores(db, list(fit_id = 4, n_factors = 2, has_loadings = TRUE)))
})

test_that("a projection decodes into weights, p values and r squared", {
  row <- data.frame(row_ids = encode_ids(c("F1", "F2")), col_ids = encode_ids(c("a", "b", "c")))
  row$data <- list(encode_f32(c(1:6, (1:6) / 10, 0.1, 0.2, 0.3)))
  p <- decode_projection(row)
  expected <- matrix(as.numeric(1:6), 2, dimnames = list(c("F1", "F2"), letters[1:3]))
  expect_equal(p$projection, expected)
  expect_equal(p$pval[2, 3], 0.6, tolerance = 1e-6)
  expect_equal(p$r_squared, c(a = 0.1, b = 0.2, c = 0.3), tolerance = 1e-6)
})

test_that("projections come from the blob, or from the file when it exists", {
  P <- load_projection(db, 1, "stability_artifacts/none.rds")
  expect_equal(dim(P), c(3, 4))
  expect_equal(nrow(load_projection(db, 1, "stability_artifacts/none.rds", n = 2)), 2)
  expect_null(load_projection(db, 99, NA_character_))
  copy <- fixture_copy()
  full <- matrix(1:4, 2, 2, dimnames = list(c("F1", "F2"), c("a", "b")))
  write_artifact(copy, "stability_artifacts/p.rds", list(projection = full))
  expect_equal(load_projection(copy, 1, "stability_artifacts/p.rds"), full)
})

test_that("trajectories need within-family projections", {
  expect_equal(nrow(within_projections(db, 1)), 0)
  expect_null(trajectory_data(db, 1, "F1"))
})

test_that("a driver file becomes the table the database keeps", {
  res <- list(
    sig_genes = list(PV_significant_shared_genes = "b"),
    normalized_weights = c(a = 0.1, b = 0.9),
    weighted_mean_stats = data.frame(
      gene = c("a", "b"), mean_diff = c(0.5, -1), welch_padj = c(0.2, 0.01)
    ),
    weighted_mean_ci = data.frame(gene = c("a", "b"), low = c(-1, 0), high = c(1, 2))
  )
  pv <- normalize_driver(res, "PV")
  expect_equal(pv$shared, c(FALSE, TRUE))
  expect_equal(pv$padj, c(0.2, 0.01))
  expect_equal(pv$weight, c(0.1, 0.9))
  ci <- normalize_driver(res, "CI")
  expect_equal(ci$low, c(-1, 0))
  expect_true(all(is.na(ci$padj)))
})

test_that("drivers come from the database copy, or from the full file", {
  d <- fit_drivers(db, 1)
  expect_equal(nrow(d), 2)
  expect_equal(nrow(fit_drivers(db, 1, 0)), 0)
  ci <- d[d$mode == "CI", ]
  res <- load_driver(db, ci)
  expect_true(from_database(res))
  expect_true(is.logical(res$shared))
  expect_true(all(res$gene %in% dataset_genes(db, "TOY1")$gene_id))
  expect_equal(sum(res$shared), 2)

  copy <- fixture_copy()
  full <- list(
    sig_genes = list(significant_shared_genes = "ENSG00000000001"),
    normalized_weights = c(ENSG00000000001 = 0.3, ENSG00000000002 = 0.7),
    weighted_mean_ci = data.frame(
      gene = c("ENSG00000000001", "ENSG00000000002"), low = c(-1, 0), high = c(1, 2)
    )
  )
  write_artifact(copy, ci$path, full)
  res <- load_driver(copy, ci)
  expect_identical(attr(res, "source"), "artifact")
  expect_equal(nrow(res), 2)
  expect_equal(res$shared, c(TRUE, FALSE))
})

test_that("sample metadata is rebuilt with its classes", {
  rows <- data.frame(
    sample_id = rep(c("s1", "s2"), 4),
    field = rep(c("age", "n", "ok", "arm"), each = 2),
    type = rep(c("numeric", "integer", "logical", "factor"), each = 2),
    value = c("1.5", NA, "3", "4", "TRUE", "FALSE", "x", "y")
  )
  m <- wide_metadata(rows)
  expect_equal(m$sample_id, c("s1", "s2"))
  expect_equal(m$age, c(1.5, NA))
  expect_identical(m$n, c(3L, 4L))
  expect_identical(m$ok, c(TRUE, FALSE))
  expect_s3_class(m$arm, "factor")
  expect_null(wide_metadata(rows[0, ]))

  fx <- dataset_sample_metadata(db, "TOY1")
  expect_equal(nrow(fx), 6)
  expect_true(all(c("sample_id", "age", "group", "subject") %in% names(fx)))
  expect_type(fx$age, "double")
  expect_type(fx$group, "character")
  expect_type(fx$subject, "integer")
})

test_that("a WGCNA gene dendrogram is rebuilt as an hclust with gene ids", {
  hc <- wgcna_dendrogram(db, 12, "TOY1")
  expect_s3_class(hc, "hclust")
  expect_equal(dim(hc$merge), c(9, 2))
  expect_length(hc$height, 9)
  expect_true(all(hc$labels %in% dataset_genes(db, "TOY1")$gene_id))
  expect_equal(sort(hc$order), 1:10)
  expect_null(wgcna_dendrogram(db, 1, "TOY1"))
})

test_that("gene variance has a logical factor-gene flag", {
  g <- gene_variance_for(db, "TOY1")
  expect_named(g, c("time_all", "factor_gene"))
  expect_type(g$factor_gene, "logical")
})
