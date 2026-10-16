fit_method <- function(method, params, mat = synthetic_expression()) {
  manifest <- method_manifest()
  registry <- manifest[[method]]$registry
  resolved <- utils::modifyList(registry$defaults, params)
  if (!is.null(registry$resource_defaults)) {
    resolved <- registry$resource_defaults(resolved, list(cpus_per_task = 1))
  }
  grid <- add_batches(registry$build_grid(resolved), 1L)
  mat_use <- if (isTRUE(registry$needs_nonneg)) shift_nonneg(mat) else mat
  subjects <- stats::setNames(colnames(mat), colnames(mat))
  list(
    fits = run_method_fits(registry, grid, mat_use, subjects),
    spec = manifest[[method]]$ingest
  )
}

extract_all <- function(x) {
  lapply(x$fits, function(f) extract_fit(x$spec, f$result, flatten_params(f$params)))
}

test_that("the method set is the closed set of six, each with a validated spec", {
  manifest <- method_manifest()
  expect_equal(names(manifest), c("pca", "spca", "ica", "nmf", "cogaps", "wgcna"))
  for (m in names(manifest)) {
    spec <- manifest[[m]]$ingest
    expect_equal(spec$method_output_version, 1L)
    expect_contains(c("seed_sweep", "param_grid"), spec$family)
    expect_type(spec$extract, "closure")
  }
  expect_equal(method_table(manifest)$method, names(manifest))
})

test_that("defaults describe a generic loadings-and-scores method", {
  spec <- method_spec("nmf")
  expect_equal(spec$representative, "min_mse_per_rank")
  expect_equal(spec$optimal, "min_mean_mse")
  expect_equal(spec$pairs, "loadings")
  expect_true(spec$redundancy)
  expect_equal(spec$markers, "generic")
  expect_false(spec$icasso)
  expect_equal(spec$projection, "loadings")
})

test_that("the per-method behaviours are the ones the source code branched on", {
  expect_equal(method_spec("pca")$representative, "max_rank")
  expect_equal(method_spec("pca")$projection, "prcomp")
  expect_false(method_spec("pca")$redundancy)
  expect_equal(method_spec("spca")$representative, "selected")
  expect_equal(method_spec("spca")$optimal_metric, "pev_sparse")
  expect_true(method_spec("ica")$icasso)
  expect_equal(method_spec("cogaps")$markers, "cogaps")
  expect_equal(method_spec("wgcna")$pairs, "modules")
  expect_equal(method_spec("wgcna")$optimal, "none")
  expect_equal(method_spec("wgcna")$factor_index, "module_id")
})

test_that("an invalid spec is rejected", {
  spec <- pca_ingest
  spec$representative <- "best"
  expect_snapshot(validate_method_spec(spec, "pca"), error = TRUE)
  spec <- pca_ingest
  spec$family <- NULL
  expect_snapshot(validate_method_spec(spec, "pca"), error = TRUE)
  spec <- spca_ingest
  spec$optimal_metric <- NA_character_
  expect_snapshot(validate_method_spec(spec, "spca"), error = TRUE)
})

test_that("PCA is fit once at the largest rank and keeps its full spectrum", {
  x <- fit_method("pca", list(rank = c(2, 3)))
  expect_length(x$fits, 1)
  rec <- extract_all(x)[[1]]
  expect_equal(rec$status, "ok")
  expect_equal(dim(rec$loadings), c(120, 3))
  expect_equal(dim(rec$scores), c(30, 3))
  expect_named(rec$diag, c("sdev", "center", "mse_by_rank"))
  expect_length(rec$diag$mse_by_rank, 3)
  expect_equal(names(rec$diag$center), rownames(rec$loadings))
  expect_equal(rec$factors$factor_index, 1:3)
})

test_that("sPCA records sparsity, convergence and no mse", {
  x <- fit_method("spca", list(K = 2, K_scan = 2, para_scan = 0.3))
  rec <- extract_all(x)[[1]]
  expect_equal(rec$status, "ok")
  expect_true(is.na(rec$mse))
  expect_equal(dim(rec$loadings), c(120, 2))
  expect_true(all(c("pev_sparse", "iterations", "converged", "fit_seconds") %in% names(rec$metrics)))
  expect_true(all(rec$factors$n_genes <= 120))
  expect_equal(rec$factors$n_genes, as.integer(colSums(rec$loadings != 0)))
})

test_that("sPCA's selection table carries fit keys that match the extracted fits", {
  manifest <- method_manifest()
  registry <- manifest$spca$registry
  meta <- list(dataset = list(id = "SYN"), methods = list(
    spca = list(K = 2:3, K_scan = 2, para_scan = c(0.3, 0.5), para_step = 0.1)
  ))
  mat <- synthetic_expression()
  subjects <- stats::setNames(colnames(mat), colnames(mat))
  grid <- build_method_grid(registry, meta, "spca", 1, 1)
  scan <- run_method_fits(registry, grid, mat, subjects)
  pca <- run_method_fits(method_registry("pca"), add_batches(data.frame(rank = 3), 1), mat, subjects)
  refine_grid <- suppressWarnings(build_refine_grid(registry, meta, "spca", 1, 1, list(scan), list(pca)))
  refine <- run_method_fits(registry, refine_grid, mat, subjects)
  selection <- suppressWarnings(select_method_fits(registry, meta, "spca", 1, list(scan), list(refine), list(pca)))
  expect_false(anyNA(selection$fit_key))
  ingest <- ingest_method_fits(c(scan, refine), manifest$spca$ingest, "spca", "SYN", withr::local_tempdir())
  expect_true(all(selection$fit_key %in% ingest$fits$fit_key))
  expect_equal(sum(selection$selected), 2)
  expect_setequal(
    representative_keys(ingest, manifest$spca$ingest, selection),
    selection$fit_key[selection$selected]
  )
  expect_true(all(c("index_of_sparseness", "is_selected") %in% selection_metric_rows(ingest, selection)$name))
})

test_that("ICA fits are independent per seed, and bootstrap fits have no scores", {
  x <- fit_method("ica", list(n.comp = 2, seed = 1, bootstrap = c(FALSE, TRUE)))
  recs <- extract_all(x)
  expect_equal(vapply(recs, `[[`, "", "status"), c("ok", "ok"))
  expect_equal(dim(recs[[1]]$loadings), c(120, 2))
  expect_false(is.null(recs[[1]]$scores))
  expect_null(recs[[2]]$scores)
  expect_true(recs[[2]]$bootstrap)
  expect_false(anyNA(recs[[1]]$factors$kurtosis))
  expect_contains(names(recs[[1]]$diag), c("W", "K", "A", "S", "prewhiten_sdev", "pc_rotation", "pc_center", "kurtosis"))
  expect_length(recs[[2]]$diag$boot_idx, 30)
})

test_that("NMF fits the non-negative shift and keeps convergence diagnostics", {
  x <- fit_method("nmf", list(k = 2, seed = 1))
  rec <- extract_all(x)[[1]]
  expect_equal(rec$status, "ok")
  expect_true(all(rec$loadings >= 0))
  expect_equal(dim(rec$scores), c(30, 2))
  expect_true(all(c("n_iteration", "target_loss") %in% names(rec$metrics)))
})

test_that("WGCNA modules are the factors, with ME scores and module assignments", {
  skip_if_not_installed("WGCNA")
  x <- fit_method("wgcna", list(power = 6, minModuleSize = 5, nThreads = 2))
  rec <- extract_all(x)[[1]]
  expect_equal(rec$status, "ok")
  expect_null(rec$loadings)
  expect_match(colnames(rec$scores), "^ME[0-9]+$")
  expect_setequal(rec$factors$factor_index, setdiff(unique(rec$modules$module), 0L))
  expect_equal(rec$factors$label, paste0("ME", rec$factors$factor_index))
  expect_true(all(c("n_unassigned", "scale_free_r2", "mean_k") %in% names(rec$metrics)))
})

test_that("CoGAPS keeps its raw result and markers come from it", {
  skip_on_cran()
  skip_if_not_installed("CoGAPS")
  x <- suppressWarnings(fit_method("cogaps", list(
    params = list(nPatterns = 2, seed = 1, nIterations = 200, distributed = "genome-wide"),
    distributed_params = list(nSets = 2), run = list(nThreads = 1)
  )))
  rec <- extract_all(x)[[1]]
  expect_equal(rec$status, "ok")
  expect_s4_class(rec$raw, "CogapsResult")
  expect_equal(dim(rec$loadings), c(120, 2))
  markers <- cogaps_pattern_markers(rec$raw)
  expect_named(markers, c("gene", "factor_index", "score"))
})

test_that("a failed fit becomes a failed record, not an error", {
  expect_equal(extract_fit(pca_ingest, list(error = "boom"), list(rank = 3))$status, "failed")
  killed <- extract_fit(spca_ingest, list(killed_not_converged = TRUE, iterations = 30, elapsed = 3600), list(K = 3))
  expect_equal(killed$status, "failed")
  expect_match(killed$error, "^killed_not_converged")
  expect_equal(killed$rank, 3L)
  expect_equal(killed$metrics[["converged"]], 0)
})

test_that("fit keys depend on the parameters but not on the resource settings", {
  a <- fit_key("D", "nmf", list(k = 5, seed = 1, n.threads = 1), "n.threads")
  b <- fit_key("D", "nmf", list(k = 5, seed = 1, n.threads = 8), "n.threads")
  c <- fit_key("D", "nmf", list(k = 6, seed = 1, n.threads = 1), "n.threads")
  expect_equal(a, b)
  expect_false(a == c)
  expect_match(a, "^nmf_[0-9a-f]{16}$")
})
