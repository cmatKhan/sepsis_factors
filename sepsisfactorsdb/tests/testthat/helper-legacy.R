# A tiny schema-3 tree: two datasets (LA, LB) with one PCA fit each, a
# projection of LA's fit onto LB, config files, and the artifact files.
local_legacy <- function(env = parent.frame()) {
  root <- withr::local_tempdir("legacy-", .local_envir = env)
  cfg <- withr::local_tempdir("config-", .local_envir = env)
  genes <- sprintf("g%d", 1:6)
  samples <- sprintf("s%d", 1:5)
  art <- function(ds, name, obj) {
    dir.create(file.path(root, "stability_artifacts", ds), recursive = TRUE, showWarnings = FALSE)
    saveRDS(obj, file.path(root, "stability_artifacts", ds, name))
    file.path("stability_artifacts", ds, name)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(root, "stability.sqlite"))
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA user_version = 3")
  dsets <- c("LA", "LB")
  keys <- c(LA = "pca_00000000000000a1", LB = "pca_00000000000000b1")
  DBI::dbWriteTable(con, "datasets", data.frame(
    dataset_id = dsets, description = "d", n_samples = 5L, n_genes = 6L, sample_id_col = "sample_id",
    feature_id_col = "feature_id", ensembl_col = "feature_id", symbol_col = "symbol", subject_id_col = NA_character_,
    sample_metadata_source = NA_character_, feature_metadata_source = NA_character_, written_at = "2026-01-01T00:00:00Z"
  ))
  DBI::dbWriteTable(con, "methods", data.frame(method = "pca", family = "seed_sweep", sign_ambiguous = 1L, has_loadings = 1L))
  DBI::dbWriteTable(con, "fits", data.frame(
    fit_id = 1:2, fit_key = unname(keys), dataset_id = dsets, method = "pca", rank = 3L, seed = NA_integer_,
    bootstrap = 0L, representative = 1L, mse = 0.1, n_factors = 3L, status = "ok", error = NA_character_,
    written_at = "2026-01-01T00:00:00Z"
  ))
  DBI::dbWriteTable(con, "factors", data.frame(
    factor_id = 1:6, fit_id = rep(1:2, each = 3), factor_index = rep(1:3, 2), label = paste0("PC", rep(1:3, 2)),
    n_genes = NA_integer_, stability_cosine = NA_real_, stability_pearson = NA_real_, stability_spearman = NA_real_,
    kurtosis = NA_real_, excess_kurtosis = NA_real_
  ))
  fa <- do.call(rbind, lapply(1:2, function(i) {
    ds <- dsets[i]
    k <- keys[[ds]]
    mat <- function(n, rn) matrix(stats::rnorm(n * 3), n, 3, dimnames = list(rn, paste0("PC", 1:3)))
    data.frame(
      fit_id = i,
      kind = c("loadings", "scores", "diag", "raw"),
      path = c(
        art(ds, paste0(k, "_loadings.rds"), mat(6, genes)),
        art(ds, paste0(k, "_scores.rds"), mat(5, samples)),
        art(ds, paste0(k, "_diag.rds"), list(sdev = c(3, 2, 1), mse_by_rank = c(0.5, 0.3, 0.1))),
        art(ds, paste0(k, "_raw.rds"), list())
      )
    )
  }))
  DBI::dbWriteTable(con, "fit_artifacts", fa)
  da <- do.call(rbind, lapply(dsets, function(ds) {
    data.frame(
      dataset_id = ds,
      kind = c("sample_metadata", "feature_metadata", "matrix"),
      path = c(
        art(ds, "sample_metadata.rds", data.frame(sample_id = samples, age = c(1, 2, 3, 4, 5))),
        art(ds, "feature_metadata.rds", data.frame(feature_id = genes, symbol = toupper(genes))),
        art(ds, "matrix.rds", matrix(0, 2, 2))
      )
    )
  }))
  DBI::dbWriteTable(con, "dataset_artifacts", da)
  pf <- art("LA", paste0("projection_", keys[["LA"]], "_tgt_LB.rds"), list(
    projection = matrix(1, 3, 5, dimnames = list(paste0("PC", 1:3), samples))
  ))
  DBI::dbWriteTable(con, "projections", data.frame(
    projection_id = 1L, source_fit_id = 1L, target_dataset_id = "LB", projection_type = "cross_dataset",
    include_intercept = 1L, relative_r2 = 0.5, path = pf
  ))
  DBI::dbWriteTable(con, "time_gene_recurrence", data.frame(symbol = "G1", n_datasets = 2L, datasets = "LA,LB"))

  dir.create(file.path(cfg, "config"))
  dir.create(file.path(cfg, "datasets", "LA"), recursive = TRUE)
  writeLines(c("FAM: [LB, LA]", "OTHER: [LZ]"), file.path(cfg, "config", "dataset_families.yml"))
  writeLines(c("neutrophil: [AAA, BBB]", "lymphocyte: [CCC]"), file.path(cfg, "config", "composition_markers.yml"))
  writeLines(c("dataset:", "  id: LA", "  hf_collection: \"org/coll\"   # note"), file.path(cfg, "datasets", "LA", "config.yml"))
  list(root = root, config = cfg, keys = keys)
}
