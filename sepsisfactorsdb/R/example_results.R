#' Write a tiny example results output
#'
#' Builds a small, valid results output for testing and for showing the
#' contract (version 2): two datasets, `TOY1` (all six methods, pairs, ICASSO,
#' WGCNA modules, enrichment, drivers, repeated-measures tables) and `TOY2`
#' (PCA only), plus cross-dataset projections and composition markers. The
#' values are synthetic.
#'
#' @param path Directory to write (created; must be empty or absent).
#' @param n_datasets 2, or 3 to add `TOY3` (PCA and NMF) and a projection of
#'   `TOY2`'s PCA fit onto it, as the parallel build's tests need.
#' @return `path`, invisibly.
#' @export
example_results <- function(path = tempfile("results-"), n_datasets = 2L) {
  if (dir.exists(path) && length(list.files(path))) {
    cli::cli_abort("{.file {path}} is not empty.")
  }
  # fit keys count up across the datasets, so each dataset starts after the fits of the last
  toy1 <- example_dataset(
    "TOY1",
    c("pca", "spca", "ica", "nmf", "cogaps", "wgcna"),
    first_n = 0L,
    path,
    full = TRUE
  )
  toy2 <- example_dataset("TOY2", "pca", first_n = toy1$n_fits, path, full = FALSE)
  toy3 <- if (n_datasets >= 3L) {
    first_n <- toy1$n_fits + toy2$n_fits
    example_dataset("TOY3", c("pca", "nmf"), first_n = first_n, path, full = FALSE)
  }

  cross_dir <- file.path(path, "cross_dataset")
  write_result_table(
    data.frame(
      source_fit_key = toy1$keys$pca,
      target_dataset_id = "TOY2",
      projection_type = "cross_dataset",
      include_intercept = 1L,
      n_genes_matched = 8L,
      n_samples = 4L,
      mean_pval = 0.1,
      median_pval = 0.05,
      mean_pvar = 0.2,
      median_pvar = 0.1,
      n_components = 3L,
      transfer_r2 = 0.4,
      native_r2 = 0.5,
      relative_r2 = 0.8,
      null_r2 = 0.01,
      path = paste0("projection_", toy1$keys$pca, "_tgt_TOY2.rds")
    ),
    "projections",
    cross_dir
  )
  proj_samples <- sprintf("TOY2_S%d", 1:4)
  proj_dim <- list(paste0("F", 1:3), proj_samples)
  saveRDS(
    list(
      projection = matrix(stats::rnorm(12), 3, 4, dimnames = proj_dim),
      pval = matrix(stats::runif(12), 3, 4, dimnames = proj_dim),
      r_squared = stats::setNames(stats::runif(4), proj_samples)
    ),
    file.path(cross_dir, "artifacts", paste0("projection_", toy1$keys$pca, "_tgt_TOY2.rds"))
  )
  write_result_table(
    data.frame(
      marker_set = c("neutrophil", "neutrophil", "lymphocyte"),
      gene = c("FCGR3B", "CXCR2", "CD3D")
    ),
    "composition_markers",
    cross_dir
  )
  write_result_table(
    data.frame(
      source_fit_key = toy1$keys$pca,
      target_fit_key = toy2$keys$pca,
      n_genes_matched = 8L,
      mean_abs_diagonal = 0.7
    ),
    "gene_space_agreement",
    cross_dir
  )
  write_result_table(
    data.frame(symbol = c("G1", "G2"), n_datasets = 2L, datasets = "TOY1,TOY2"),
    "time_gene_recurrence",
    cross_dir
  )

  if (!is.null(toy3)) {
    proj3 <- paste0("projection_", toy2$keys$pca, "_tgt_TOY3.rds")
    proj_dim3 <- list(paste0("F", 1:3), sprintf("TOY3_S%d", 1:4))
    saveRDS(
      list(
        projection = matrix(stats::rnorm(12), 3, 4, dimnames = proj_dim3),
        pval = matrix(stats::runif(12), 3, 4, dimnames = proj_dim3),
        r_squared = stats::setNames(stats::runif(4), proj_dim3[[2]])
      ),
      file.path(cross_dir, "artifacts", proj3)
    )
    tab <- as.data.frame(arrow::read_parquet(file.path(cross_dir, "tables", "projections.parquet")))
    row3 <- tab
    row3$source_fit_key <- toy2$keys$pca
    row3$target_dataset_id <- "TOY3"
    row3$path <- proj3
    write_result_table(rbind(tab, row3), "projections", cross_dir)
  }

  methods <- stats::setNames(
    lapply(names(SUPPORTED_METHOD_OUTPUTS), function(m) list(method_output_version = 1L)),
    names(SUPPORTED_METHOD_OUTPUTS)
  )
  jsonlite::write_json(
    list(
      results_output_version = RESULTS_OUTPUT_VERSION,
      writer = list(package = "sepsisfactorsdb", version = "example"),
      created_at = utc_now(),
      datasets = c("TOY1", "TOY2", if (!is.null(toy3)) "TOY3"),
      methods = methods
    ),
    file.path(path, "manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
  invisible(path)
}

#' Write one results table
#'
#' @param df The table.
#' @param table Its name.
#' @param dir `datasets/<id>` or `cross_dataset`.
#' @noRd
write_result_table <- function(df, table, dir) {
  expected <- contract_columns(table)
  missing <- setdiff(expected, names(df))
  if (length(missing)) {
    cli::cli_abort("Example table {.val {table}} lacks {.field {missing}}.")
  }
  dir.create(file.path(dir, "tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "artifacts"), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(df, file.path(dir, "tables", paste0(table, ".parquet")))
  invisible(df)
}

example_fit_key <- function(method, n) {
  sprintf("%s_%016x", method, 4096L + 977L * n)
}

#' The fits of an example dataset, in the order they are made
#'
#' @param methods Method names.
#' @return A list with one list per fit (`method`, `rank`, `seed`, ...).
#' @noRd
example_fit_specs <- function(methods) {
  fit <- function(
    method,
    rank,
    seed = NA,
    bootstrap = 0L,
    status = "ok",
    representative = 0L,
    n_factors = rank,
    error = NA_character_,
    kinds = SUPPORTED_METHOD_OUTPUTS[[method]]$fit_artifacts
  ) {
    list(
      method = method,
      rank = rank,
      seed = seed,
      bootstrap = bootstrap,
      status = status,
      representative = representative,
      n_factors = n_factors,
      error = error,
      kinds = kinds
    )
  }
  per_method <- lapply(methods, function(m) {
    switch(m,
      pca = list(fit("pca", 3, representative = 1L)),
      spca = list(
        fit("spca", 2, representative = 1L),
        fit("spca", 2),
        fit("spca", 2, status = "failed", error = "killed_not_converged (8h)")
      ),
      ica = list(
        fit("ica", 2, seed = 1L, representative = 1L),
        fit("ica", 2, seed = 2L),
        fit("ica", 2, seed = 3L, bootstrap = 1L)
      ),
      nmf = list(
        fit("nmf", 2, seed = 1L, representative = 1L),
        fit("nmf", 2, seed = 2L)
      ),
      cogaps = list(
        fit("cogaps", 2, seed = 1L, representative = 1L),
        fit("cogaps", 2, seed = 2L)
      ),
      wgcna = list(
        fit(
          "wgcna",
          NA,
          representative = 1L,
          n_factors = 2,
          kinds = c("scores", "diag", "dendrogram")
        ),
        fit("wgcna", NA, n_factors = 2)
      )
    )
  })
  unlist(per_method, recursive = FALSE)
}

#' One synthetic fit: its `fits` row, `factors` rows and artifact rows
#'
#' Writes the fit's artifacts with `art()`.
#'
#' @noRd
example_fit <- function(spec, key, id, genes, samples, full, art) {
  method <- spec$method
  n_factors <- spec$n_factors
  status <- spec$status
  fit <- data.frame(
    fit_key = key,
    dataset_id = id,
    method = method,
    rank = as.integer(spec$rank),
    seed = as.integer(spec$seed),
    bootstrap = as.integer(spec$bootstrap),
    representative = as.integer(spec$representative),
    mse = if (method == "spca" || method == "wgcna") NA_real_ else 0.5,
    n_factors = if (status == "ok") as.integer(n_factors) else NA_integer_,
    status = status,
    error = spec$error
  )
  if (status != "ok") {
    return(list(fit = fit, factors = NULL, artifacts = NULL))
  }
  idx <- seq_len(n_factors)
  labels_f <- paste0(if (method == "wgcna") "ME" else "F", idx)
  factors <- data.frame(
    fit_key = key,
    factor_index = idx,
    label = labels_f,
    n_genes = if (method == "wgcna") 5L else 10L,
    stability_cosine = if (full && method == "ica") 0.9 else NA_real_,
    stability_pearson = if (full && method == "ica") 0.88 else NA_real_,
    stability_spearman = if (full && method == "ica") 0.85 else NA_real_,
    kurtosis = if (method == "ica") 3.1 else NA_real_,
    excess_kurtosis = if (method == "ica") 0.1 else NA_real_
  )
  colnames_f <- paste0("F", idx)
  # a bootstrap fit has no scores artifact, but its random numbers are still drawn, so the later
  # artifacts keep the values they always had
  artifacts <- lapply(spec$kinds, function(kind) {
    obj <- switch(kind,
      loadings = matrix(
        stats::rnorm(10 * n_factors),
        10,
        n_factors,
        dimnames = list(genes, colnames_f)
      ),
      scores = matrix(
        stats::rnorm(6 * n_factors),
        6,
        n_factors,
        dimnames = list(samples, labels_f)
      ),
      dendrogram = {
        h <- stats::hclust(stats::dist(matrix(stats::rnorm(20), 10)))
        list(merge = h$merge, height = h$height, order = h$order, labels = genes)
      },
      diag = list(
        sdev = c(2, 1, 0.5),
        center = stats::setNames(rep(0, 10), genes),
        mse_by_rank = c(1, 0.6, 0.3)
      ),
      list()
    )
    if (kind == "scores" && spec$bootstrap == 1L) {
      return(NULL)
    }
    data.frame(fit_key = key, kind = kind, path = art(paste0(key, "_", kind, ".rds"), obj))
  })
  list(fit = fit, factors = factors, artifacts = do.call(rbind, artifacts))
}

#' One synthetic dataset of the example results
#'
#' @noRd
example_dataset <- function(id, methods, first_n, root, full) {
  set.seed(match(id, c("TOY1", "TOY2", "TOY3")))
  dir <- file.path(root, "datasets", id)
  dir.create(file.path(dir, "artifacts"), recursive = TRUE, showWarnings = FALSE)
  genes <- sprintf("ENSG%011d", 1:10)
  samples <- sprintf("%s_S%d", id, 1:6)
  art <- function(name, obj) {
    saveRDS(obj, file.path(dir, "artifacts", name))
    name
  }
  tbl <- function(df, table) write_result_table(df, table, dir)

  specs <- example_fit_specs(methods)
  spec_methods <- vapply(specs, function(spec) spec$method, "")
  fit_keys <- example_fit_key(spec_methods, first_n + seq_along(specs))
  built <- Map(
    function(spec, key) example_fit(spec, key, id, genes, samples, full, art),
    specs,
    fit_keys
  )
  keys <- split(fit_keys, factor(spec_methods, levels = unique(spec_methods)))
  fits <- do.call(rbind, lapply(built, `[[`, "fit"))
  tbl(fits, "fits")
  tbl(do.call(rbind, lapply(built, `[[`, "factors")), "factors")

  method_rows <- do.call(
    rbind,
    lapply(methods, function(m) {
      s <- SUPPORTED_METHOD_OUTPUTS[[m]]
      data.frame(
        method = m,
        family = s$family,
        sign_ambiguous = as.integer(s$sign_ambiguous),
        has_loadings = as.integer(s$has_loadings)
      )
    })
  )
  tbl(method_rows, "methods")

  dataset_files <- data.frame(
    kind = c("matrix", "sample_metadata", "feature_metadata"),
    path = c(
      art("matrix.rds", matrix(stats::rnorm(60), 10, 6, dimnames = list(genes, samples))),
      art(
        "sample_metadata.rds",
        data.frame(
          sample_id = samples,
          subject = rep(1:3, 2),
          age = c(30, 41, 52, 63, 34, 45),
          group = rep(c("a", "b"), each = 3)
        )
      ),
      art(
        "feature_metadata.rds",
        data.frame(feature_id = genes, ensembl = genes, symbol = paste0("SYM", 1:10))
      )
    )
  )
  tbl(data.frame(dataset_id = id, dataset_files), "dataset_artifacts")
  tbl(
    data.frame(
      dataset_id = id,
      description = paste("Synthetic dataset", id),
      n_samples = 6L,
      n_genes = 10L,
      sample_id_col = "sample_id",
      feature_id_col = "feature_id",
      ensembl_col = "ensembl",
      symbol_col = "symbol",
      subject_id_col = "subject",
      sample_metadata_source = NA_character_,
      feature_metadata_source = NA_character_,
      family = id,
      family_position = 1L,
      hf_collection = "toy/collection"
    ),
    "datasets"
  )
  tbl(do.call(rbind, lapply(built, `[[`, "artifacts")), "fit_artifacts")
  tbl(
    data.frame(
      fit_key = fits$fit_key[fits$status == "ok"],
      name = "n_iter",
      value = 10
    ),
    "fit_metrics"
  )
  tbl(
    data.frame(
      fit_key = fits$fit_key,
      name = "rank",
      num_value = as.numeric(fits$rank),
      text_value = NA_character_
    ),
    "fit_params"
  )

  if (full) {
    example_full_tables(id, keys, fits, genes, samples, tbl, art)
  }
  invisible(list(keys = lapply(keys, `[`, 1L), n_fits = length(fit_keys)))
}

#' The analysis tables of the example's full dataset
#'
#' @noRd
example_full_tables <- function(id, keys, fits, genes, samples, tbl, art) {
  # pairs between the two ICA seed fits and the two WGCNA fits
  ica <- keys$ica[1:2]
  wgcna <- keys$wgcna[1:2]
  tbl(
    data.frame(
      fit_a_key = c(ica[2], wgcna[1]),
      fit_b_key = c(ica[1], wgcna[2]), # given in the "wrong" order to exercise the swap
      same_rank = c(1L, 0L),
      n_factors_a = 2L,
      n_factors_b = 2L,
      ari = c(NA, 0.8)
    ),
    "fit_pairs"
  )
  tbl(
    data.frame(
      fit_a_key = ica[2],
      fit_b_key = ica[1],
      factor_a_index = 1:2,
      factor_b_index = 2:1
    ),
    "factor_matches"
  )
  metrics <- c("cosine", "pearson", "spearman")
  scores <- expand.grid(factor_a_index = 1:2, metric = metrics, stringsAsFactors = FALSE)
  tbl(
    data.frame(
      fit_a_key = ica[2],
      fit_b_key = ica[1],
      scores,
      value = 0.9,
      runner_up = 0.2,
      margin = 0.7
    ),
    "match_scores"
  )
  tbl(
    data.frame(
      fit_a_key = ica[2],
      fit_b_key = ica[1],
      metric = metrics,
      n = 100L,
      mean = 0.01,
      sd = 0.05,
      median = 0.01,
      p95 = 0.1,
      max = 0.3
    ),
    "fit_pair_null"
  )
  tbl(
    data.frame(
      dataset_id = id,
      method = "ica",
      level_a = 2L,
      level_b = 2L,
      metric = "cosine",
      bin = 1:3,
      bin_lo = c(0, 0.5, 0.9),
      bin_hi = c(0.5, 0.9, 1),
      n_matched = 1L,
      n_unmatched = 2L
    ),
    "similarity_histograms"
  )

  # WGCNA
  gm <- expand.grid(gene = genes, fit_key = wgcna, stringsAsFactors = FALSE)
  gm$module <- rep(1:2, length.out = nrow(gm))
  gm$kme_own <- 0.8
  gm$next_module <- NA_integer_
  gm$kme_next <- NA_real_
  gm$margin <- NA_real_
  gm$p_analytic <- 0.01
  gm$p_perm <- 0.02
  tbl(
    gm[, c(
      "fit_key",
      "gene",
      "module",
      "kme_own",
      "next_module",
      "kme_next",
      "margin",
      "p_analytic",
      "p_perm"
    )],
    "gene_modules"
  )
  ks <- expand.grid(
    fit_key = wgcna,
    module = 1:2,
    grp = c("member", "nonmember", "permutation"),
    stringsAsFactors = FALSE
  )
  tbl(
    data.frame(ks, n = 5L, mean = 0.5, sd = 0.1, median = 0.5, p95 = 0.7, p99 = 0.8),
    "module_kme_summary"
  )
  tbl(
    data.frame(ks, bin = 1L, bin_lo = 0, bin_hi = 0.1, count = 3L),
    "module_kme_histograms"
  )
  tbl(
    data.frame(
      dataset_id = id,
      power = c(4L, 6L, 8L),
      sft_r_sq = 0.8,
      slope = -1.5,
      truncated_r_sq = 0.85,
      mean_k = 10,
      median_k = 8,
      max_k = 30
    ),
    "wgcna_sft"
  )
  tbl(
    data.frame(
      dataset_id = id,
      field = "time",
      gene = genes[1:3],
      test = "spearman",
      statistic = 0.3,
      p_value = 0.2
    ),
    "gene_significance"
  )

  # ICASSO: two clusters, every factor of the three ICA fits belongs to one
  ica_all <- keys$ica
  tbl(
    data.frame(
      dataset_id = id,
      rank = 2L,
      cluster_id = 1:2,
      iq = c(0.9, 0.8),
      n_members = 3L,
      centrotype_fit_key = ica[1],
      centrotype_factor_index = 1:2,
      dendro_file = art("icasso_dendro_rank2.rds", list(merge = matrix(1:2, 1)))
    ),
    "icasso_clusters"
  )
  mb <- expand.grid(fit_key = ica_all, factor_index = 1:2, stringsAsFactors = FALSE)
  tbl(
    data.frame(mb, dataset_id = id, rank = 2L, cluster_id = mb$factor_index, intra_sim = 0.9),
    "icasso_membership"
  )

  # redundancy (CoGAPS), drivers, enrichment, factor variance
  tbl(
    data.frame(
      fit_key = keys$cogaps[1],
      max_offdiag_cosine = 0.3,
      median_offdiag_cosine = 0.1,
      n_factors_with_no_markers = 0L
    ),
    "fit_redundancy"
  )
  weights <- stats::setNames(stats::rnorm(10), genes)
  stats_df <- data.frame(
    mean_diff = stats::rnorm(10),
    welch_padj = c(0.001, 0.2, 0.03, rep(0.5, 7)),
    gene = genes
  )
  ci_df <- data.frame(low = stats::rnorm(10) - 1, high = stats::rnorm(10) + 1, gene = genes)
  tbl(
    data.frame(
      fit_key = keys$pca[1],
      factor_index = 1L,
      grouping_col = "group",
      group1_level = "a",
      group2_level = "b",
      mode = c("CI", "PV"),
      n_genes_considered = 10L,
      n_significant_shared = 2L,
      path = c(
        art(
          "driver_pca_f1_group_a-vs-b_CI.rds",
          list(
            weighted_mean_ci = ci_df,
            normalized_weights = weights,
            sig_genes = list(significant_shared_genes = genes[1:2])
          )
        ),
        art(
          "driver_pca_f1_group_a-vs-b_PV.rds",
          list(
            weighted_mean_stats = stats_df,
            normalized_weights = weights,
            sig_genes = list(PV_significant_shared_genes = genes[1:2])
          )
        )
      )
    ),
    "pattern_drivers"
  )
  tbl(
    data.frame(
      fit_key = keys$pca[1],
      factor_index = 1:2,
      query_type = "gsea_hallmark",
      direction = "pos",
      query_size = 10L
    ),
    "enrichment_queries"
  )
  tbl(
    data.frame(
      fit_key = keys$pca[1],
      factor_index = 1:2,
      query_type = "gsea_hallmark",
      direction = "pos",
      term_id = "HALLMARK_X",
      source = "HALLMARK",
      term_name = "Hallmark X",
      p_value = 0.01,
      intersection_size = NA_integer_,
      term_size = 20L,
      nes = 1.5,
      es = 0.6,
      log2err = 0.4,
      is_main_pathway = 1L
    ),
    "enrichment_results"
  )
  tbl(
    data.frame(dataset_id = id, method = "pca", formula = "~ (1|subject)", n_factors = 3L),
    "factor_variance_models"
  )
  tbl(
    data.frame(
      fit_key = keys$pca[1],
      factor_index = 1L,
      term = c("subject", "Residuals"),
      category = c("between_subject", "residual"),
      fraction = c(0.7, 0.3)
    ),
    "factor_variance"
  )

  # dataset-level QC and repeated-measures tables
  tbl(
    data.frame(
      dataset_id = id,
      val_min = 0, val_max = 10, val_mean = 5, val_sd = 2,
      skew_p50 = 0, skew_p95 = 1, kurtosis_p50 = 3, kurtosis_p95 = 4,
      min_singular_value = 0.1, max_singular_value = 9, condition_number = 90, max_sample_cor = 0.99
    ),
    "matrix_diagnostics"
  )
  tbl(
    data.frame(
      dataset_id = id, parent_dataset_id = id, is_subset = 0L, platform = "array",
      subject_col = "subject", time_col = "time", time_order = "t0,t1", group_col = NA_character_,
      covariates = NA_character_, technical_covariates = NA_character_,
      vp_formula = "~ (1|subject)", dream_formula = "~ 0 + cell + (1|subject)",
      n_samples = 6L, n_subjects = 3L, n_dropped = 0L
    ),
    "dataset_design"
  )
  tbl(
    data.frame(
      dataset_id = id, sample_id = samples, subject = rep(c("a", "b", "c"), 2),
      timepoint = rep(c("t0", "t1"), each = 3), grp = NA_character_, cell = NA_character_,
      expr_mean = 5, expr_sd = 1, expr_q10 = 4, expr_median = 5, expr_q90 = 6,
      pc1 = 0.1, pc2 = 0.2, pc3 = 0.3, neutrophil_score = 0.1, lymphocyte_score = 0.2,
      nl_score = -0.1,
      control_mean = NA_real_, control_factor = NA_real_, log2_lib_size = NA_real_,
      norm_factor = NA_real_,
      chrY_score = NA_real_, xist = NA_real_, inferred_sex = NA_character_,
      recorded_sex = NA_character_
    ),
    "samples"
  )
  summary_cols <- setdiff(contract_columns("variance_summary"), "dataset_id")
  vs <- as.data.frame(stats::setNames(rep(list(NA_real_), length(summary_cols)), summary_cols))
  vs$control_probes <- NA_character_
  tbl(data.frame(dataset_id = id, vs), "variance_summary")
  tbl(
    data.frame(
      dataset_id = id, term = "subject", category = "between_subject", n_genes = 10L,
      mean = 0.5, q10 = 0.1, median = 0.5, q90 = 0.8, share_gt05 = 0.4, share_gt10 = 0.2
    ),
    "variance_terms"
  )
  tbl(
    data.frame(
      dataset_id = id, category = "between_subject", present = 1L, mean = 0.5, median = 0.5,
      vw_all = 0.5, vw_factor_genes = 0.5, vw_top = 0.5
    ),
    "variance_categories"
  )
  tbl(
    data.frame(
      dataset_id = id, binning = "expression_quartile", bin = "Q1", n_genes = 3L,
      between_subject = 0.5, time = 0.1, residual = 0.4
    ),
    "variance_bins"
  )
  tbl(
    data.frame(
      dataset_id = id, trait = "neutrophil_score", term = "subject", category = "between_subject",
      fraction = 0.6
    ),
    "trait_variance"
  )
  tbl(
    data.frame(dataset_id = id, x = "pc1", y = "nl_score", rho = 0.4, p_value = 0.3),
    "composition_correlations"
  )
  tbl(
    data.frame(
      dataset_id = id, contrast = "t1-t0", kind = "pairwise", readable = "t1 vs t0",
      n_genes = 10L, n_fdr05 = 1L
    ),
    "de_contrasts"
  )
  tbl(
    data.frame(dataset_id = id, symbol = c("G1", "G2"), time_all = 0.2, between_subject = 0.5),
    "variance_time_genes"
  )
  invisible(NULL)
}
