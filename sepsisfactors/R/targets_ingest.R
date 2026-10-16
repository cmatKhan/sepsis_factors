# Extraction: what is written about a dataset's fits, computed from the fit
# targets. Rows are keyed by `fit_key` (and factor_index), the natural keys of
# the results output; sepsisfactorsdb resolves them to ids. Per-method
# knowledge lives in each method's output spec (`method_spec()`), whose
# extract() returns fit_record_from() below.

#' Flatten one grid row into a named list of scalars
#'
#' List-columns (CoGAPS's `params`, `distributed_params`, `run`) are
#' unwrapped: the `params` column's entries keep their names, other
#' list-columns are prefixed (`run.nThreads`).
#'
#' @param params_row One grid row (data frame).
#' @return Named list of parameter values, without the batching column.
flatten_params <- function(params_row) {
  out <- list()
  for (col in setdiff(names(params_row), GRID_BATCH_COLUMNS)) {
    v <- params_row[[col]]
    if (is.list(v)) {
      inner <- v[[1]]
      if (length(inner) == 0) next
      names(inner) <- if (col == "params") names(inner) else paste(col, names(inner), sep = ".")
      out <- c(out, inner)
    } else {
      out[[col]] <- v
    }
  }
  out
}

#' Stable identity for a fit
#'
#' Also names the fit's artifact files, and is unique across datasets.
#'
#' @param dataset_id Dataset id.
#' @param method Method name.
#' @param params flatten_params() output.
#' @param resource_params Parameter names that only set parallelism, left
#'   out of the identity.
#' @return `"<method>_<16 hex characters>"`, a hash of the dataset, method and
#'   remaining parameters.
fit_key <- function(dataset_id, method, params, resource_params = character(0)) {
  p <- params[setdiff(names(params), resource_params)]
  p <- p[order(names(p))]
  paste0(method, "_", substr(rlang::hash(list(dataset_id, method, p)), 1, 16))
}

#' The standard record an `<name>_ingest$extract()` returns
#'
#' Factors come from the loadings' columns unless the method sets `factors`
#' itself (WGCNA modules). Loadings or scores without row names are dropped.
#'
#' @param rank,seed Requested rank and seed (`NA` if not applicable).
#' @param bootstrap Whether the fit was on resampled samples (ICA).
#' @param mse Reconstruction MSE, or `NA`.
#' @param loadings Genes x factors matrix.
#' @param scores Samples x factors matrix.
#' @param diag Diagnostics list (the fit's `diag` artifact).
#' @param raw The raw model object (the `raw` artifact).
#' @param dendrogram Gene tree (WGCNA; the `dendrogram` artifact).
#' @param modules Data frame (gene, module) (WGCNA).
#' @param metrics Named numeric of scalar metrics (`fit_metrics`).
#' @param factor_n_genes Per-factor gene counts (e.g. sPCA non-zeros).
#' @param factor_kurtosis Per-factor kurtosis (ICA).
#' @return A record list with `status = "ok"`.
fit_record_from <- function(rank = NA, seed = NA, bootstrap = FALSE, mse = NA,
                            loadings = NULL, scores = NULL, diag = NULL, raw = NULL,
                            dendrogram = NULL, modules = NULL, metrics = NULL,
                            factor_n_genes = NULL, factor_kurtosis = NULL) {
  as_named_matrix <- function(m) {
    if (is.null(m)) return(NULL)
    m <- as.matrix(m)
    if (is.null(rownames(m))) NULL else m
  }
  loadings <- as_named_matrix(loadings)
  scores <- as_named_matrix(scores)
  factors <- NULL
  if (!is.null(loadings)) {
    k <- ncol(loadings)
    factors <- data.frame(
      factor_index = seq_len(k),
      label = colnames(loadings) %||% paste0("Factor_", seq_len(k)),
      n_genes = if (length(factor_n_genes) == k) as.integer(factor_n_genes) else NA_integer_,
      kurtosis = if (length(factor_kurtosis) == k) factor_kurtosis else NA_real_,
      excess_kurtosis = if (length(factor_kurtosis) == k) factor_kurtosis - 3 else NA_real_)
  }
  list(status = "ok", error = NA_character_,
       rank = as.integer(rank %||% NA), seed = suppressWarnings(as.integer(seed %||% NA)),
       bootstrap = isTRUE(bootstrap), mse = as.numeric(mse %||% NA),
       n_factors = if (!is.null(loadings)) ncol(loadings) else NA_integer_,
       loadings = loadings, scores = scores, diag = diag, raw = raw, dendrogram = dendrogram,
       modules = modules, metrics = metrics, factors = factors)
}

#' A failed fit's record
#'
#' @param error Error message.
#' @param rank,seed,bootstrap The fit's requested parameters.
#' @return A fit_record_from() record with `status = "failed"` and `error`.
fit_failed <- function(error, rank = NA, seed = NA, bootstrap = FALSE) {
  rec <- fit_record_from(rank = rank, seed = seed, bootstrap = bootstrap)
  rec$status <- "failed"
  rec$error <- error
  rec
}

#' One fit's record
#'
#' Failures captured at fit time (capture_fit()'s `error`, sPCA's
#' `killed_not_converged`), an extract() error, or a loadings method without
#' loadings all become failed records rather than stopping the dataset's
#' ingestion. Adds the fit's wall time as metric `fit_seconds`.
#'
#' @param spec The method's `<name>_ingest`.
#' @param result The fit function's result.
#' @param params flatten_params() output.
#' @return A fit_record_from() record.
extract_fit <- function(spec, result, params) {
  rank <- if (is.na(spec$rank_param)) NA else params[[spec$rank_param]] %||% NA
  if (is.null(result)) return(fit_failed("no result", rank = rank, seed = params$seed))
  if (!is.null(result$error)) return(fit_failed(result$error, rank = rank, seed = params$seed))
  if (isTRUE(result$killed_not_converged)) {
    rec <- fit_failed(sprintf("killed_not_converged (stopped after %.1f h, %s iterations reached)",
                              (result$elapsed %||% NA) / 3600, format(result$iterations %||% NA)),
                      rank = rank, seed = params$seed)
    rec$metrics <- c(iterations = result$iterations %||% NA_real_, converged = 0,
                     fit_seconds = as.numeric(result$elapsed %||% NA))
    return(rec)
  }
  rec <- tryCatch(spec$extract(result, params),
                  error = function(e) fit_failed(paste("extract failed:", conditionMessage(e)), rank = rank))
  if (rec$status == "ok" && is.null(rec$loadings) && isTRUE(spec$has_loadings)) {
    rec <- fit_failed("no loadings with gene rownames", rank = rank, seed = params$seed)
  }
  if (!is.null(result$elapsed)) rec$metrics <- c(rec$metrics, fit_seconds = as.numeric(result$elapsed))
  rec
}

#' Long parameter rows
#'
#' @param params Named list of parameter values.
#' @return Data frame (name, num_value, text_value): numeric and logical
#'   values in `num_value`, others in `text_value`; `NULL` if empty.
param_rows <- function(params) {
  if (length(params) == 0) return(NULL)
  is_num <- vapply(params, function(v) is.numeric(v) || is.logical(v), logical(1))
  data.frame(name = names(params),
             num_value = ifelse(is_num, vapply(params, function(v) if (is.numeric(v) || is.logical(v)) as.numeric(v[1]) else NA_real_, numeric(1)), NA_real_),
             text_value = ifelse(is_num, NA_character_, vapply(params, function(v) as.character(v[1]), character(1))))
}

#' Extract one method's fits for a dataset (the `ingest_<m>_<dataset>` target)
#'
#' Writes each fit's artifacts as `stability_artifacts/<dataset>/<fit_key>_<kind>.rds`
#' (loadings, scores, diag, raw, dendrogram).
#'
#' @param fits List of `list(params, result)` (flatten_fits()).
#' @param spec The method's `<name>_ingest`.
#' @param method Method name.
#' @param dataset_id Dataset id.
#' @param results_root The results directory.
#' @return `list(dataset_id, method, fits, params, metrics, artifacts, factors,
#'   modules)`: data frames keyed by fit_key.
#' @keywords internal
#' @export
ingest_method_fits <- function(fits, spec, method, dataset_id, results_root) {
  art_dir <- artifacts_dir(results_root, dataset_id)
  dir.create(art_dir, recursive = TRUE, showWarnings = FALSE)

  recs <- lapply(fits, function(f) {
    params <- flatten_params(f$params)
    key <- fit_key(dataset_id, method, params, spec$resource_params)
    rec <- extract_fit(spec, f$result, params)
    arts <- list()
    for (kind in c("loadings", "scores", "diag", "raw", "dendrogram")) {
      obj <- rec[[kind]]
      if (is.null(obj)) next
      fname <- sprintf("%s_%s.rds", key, kind)
      saveRDS(obj, file.path(art_dir, fname))
      arts[[kind]] <- fname
    }
    list(key = key, rec = rec, arts = arts,
         params = param_rows(params[setdiff(names(params), spec$resource_params)]))
  })
  keys <- vapply(recs, `[[`, character(1), "key")
  if (anyDuplicated(keys)) {
    stop(dataset_id, " ", method, ": duplicate fit keys (identical parameter rows): ",
         paste(unique(keys[duplicated(keys)]), collapse = ", "))
  }

  keyed <- function(get) {
    rows <- Filter(Negate(is.null), lapply(recs, function(r) {
      df <- get(r)
      if (is.null(df) || nrow(df) == 0) NULL else cbind(data.frame(fit_key = rep(r$key, nrow(df))), df)
    }))
    if (length(rows)) do.call(rbind, rows) else NULL
  }
  list(
    dataset_id = dataset_id, method = method,
    fits = do.call(rbind, lapply(recs, function(r) with(r$rec, data.frame(
      fit_key = r$key, rank = rank, seed = seed, bootstrap = as.integer(bootstrap), mse = mse,
      n_factors = n_factors, status = status, error = error)))),
    params = keyed(function(r) r$params),
    metrics = keyed(function(r) {
      m <- r$rec$metrics
      if (length(m)) data.frame(name = names(m), value = as.numeric(m))
    }),
    artifacts = keyed(function(r) {
      if (length(r$arts)) data.frame(kind = names(r$arts), path = unlist(r$arts, use.names = FALSE))
    }),
    factors = keyed(function(r) if (r$rec$status == "ok") r$rec$factors),
    modules = keyed(function(r) if (r$rec$status == "ok") r$rec$modules))
}

#' Absolute path of a fit's artifact
#'
#' @param ingest An ingest_method_fits() result.
#' @param key fit_key.
#' @param kind Artifact kind (`"loadings"`, `"scores"`, ...).
#' @param results_root The results directory.
#' @return The absolute path, or `NA` if the fit has none.
fit_artifact <- function(ingest, key, kind, results_root) {
  a <- ingest$artifacts
  hit <- a$path[a$fit_key == key & a$kind == kind]
  if (length(hit) == 0) NA_character_ else file.path(artifacts_dir(results_root, ingest$dataset_id), hit[1])
}

#' A method's ok fits that have loadings
#'
#' @param ingest An ingest_method_fits() result.
#' @param results_root The results directory.
#' @return `ingest$fits` rows with status ok and a `loadings_file` column
#'   (absolute path).
fits_with_loadings <- function(ingest, results_root) {
  f <- ingest$fits[ingest$fits$status == "ok", , drop = FALSE]
  f$loadings_file <- vapply(f$fit_key, fit_artifact, character(1), ingest = ingest, kind = "loadings",
                            results_root = results_root)
  f[!is.na(f$loadings_file), , drop = FALSE]
}

#' One parameter's numeric value per fit
#'
#' @param ingest An ingest_method_fits() result.
#' @param name Parameter name (e.g. `"para"`, `"power"`).
#' @return Named numeric (names = fit_key), `NA` where unset.
fit_param_value <- function(ingest, name) {
  p <- ingest$params[ingest$params$name == name, , drop = FALSE]
  stats::setNames(p$num_value[match(ingest$fits$fit_key, p$fit_key)], ingest$fits$fit_key)
}

#' Pair stability for one method (the `pairs_<m>_<dataset>` target)
#'
#' @param ingest An ingest_method_fits() result.
#' @param spec The method's `<name>_ingest` (`sign_ambiguous`).
#' @param results_root The results directory.
#' @return compute_factor_pairs() or, for WGCNA, compute_wgcna_pairs() output.
#' @keywords internal
#' @export
compute_method_pairs <- function(ingest, spec, results_root) {
  if (spec$pairs == "modules") {
    mods <- ingest$modules
    if (is.null(mods)) return(NULL)
    power <- fit_param_value(ingest, "power")
    keys <- unique(mods$fit_key)
    return(compute_wgcna_pairs(split(mods[, c("gene", "module")], mods$fit_key)[keys], keys,
                               levels = unname(power[keys])))
  }
  f <- fits_with_loadings(ingest, results_root)
  compute_factor_pairs(f$fit_key, f$rank, f$loadings_file, sign_ambiguous = isTRUE(spec$sign_ambiguous))
}

#' A method's representative fits
#'
#' Used by every downstream analysis. Which fits are representative is the
#' method's `representative` rule: the dataset's selection table (sPCA's
#' Index-of-Sparseness choice), the fit at the largest rank (PCA, fit once),
#' every ok fit (WGCNA), or the lowest-mse seed at each rank. Bootstrap
#' resamples are never representative.
#'
#' @param ingest An ingest_method_fits() result.
#' @param spec The method's output spec.
#' @param selection The dataset's selection table (`selection_<method>_<dataset>`), or `NULL`.
#' @return Character vector of fit_keys.
#' @keywords internal
#' @export
representative_keys <- function(ingest, spec, selection = NULL) {
  f <- ingest$fits[ingest$fits$status == "ok" & ingest$fits$bootstrap == 0, , drop = FALSE]
  if (nrow(f) == 0) return(character(0))
  switch(spec$representative,
    selected = {
      sel <- selection[selection$selected, , drop = FALSE]
      intersect(sel$fit_key, f$fit_key)
    },
    max_rank = f$fit_key[which.max(f$rank)],
    all_ok = f$fit_key,
    min_mse_per_rank = unname(unlist(lapply(split(f, f$rank), function(r) r$fit_key[which.min(r$mse)])))
  )
}

#' sPCA's Index-of-Sparseness table as `fit_metrics` rows
#'
#' @param ingest An ingest_method_fits() result.
#' @param selection The selection table, or `NULL`.
#' @return Data frame (fit_key, name, value) with `pev_pca`, `prop_sparse`,
#'   `index_of_sparseness`, `is_selected`, `hit_max_iter` and `at_scan_edge`,
#'   or `NULL`.
selection_metric_rows <- function(ingest, selection) {
  if (is.null(selection) || nrow(selection) == 0) return(NULL)
  s <- selection[selection$fit_key %in% ingest$fits$fit_key, , drop = FALSE]
  keys <- s$fit_key
  vals <- list(pev_pca = s$PEV_pca, prop_sparse = s$PS, index_of_sparseness = s$IS,
               is_selected = as.numeric(s$selected), hit_max_iter = as.numeric(s$hit_max_iter),
               at_scan_edge = as.numeric(s$at_scan_edge))
  do.call(rbind, lapply(names(vals), function(n) data.frame(fit_key = keys, name = n, value = vals[[n]])))
}

#' Redundancy diagnostics for a method's representative fits
#'
#' Saves each fit's K x K within-fit cosine matrix as its `redundancy`
#' artifact.
#'
#' @param ingest An ingest_method_fits() result.
#' @param representatives Representative fit_keys.
#' @param results_root The results directory.
#' @return `list(pattern_markers, fit_redundancy, artifacts)`, keyed by fit_key.
#' @keywords internal
#' @export
compute_method_redundancy <- function(ingest, representatives, results_root) {
  art_dir <- artifacts_dir(results_root, ingest$dataset_id)
  markers <- list(); redundancy <- list(); arts <- list()
  for (key in representatives) {
    out <- run_redundancy_for_fit(method_spec(ingest$method)$markers, fit_artifact(ingest, key, "loadings", results_root),
                                  fit_artifact(ingest, key, "raw", results_root))
    if (is.null(out)) next
    fname <- sprintf("%s_redundancy.rds", key)
    saveRDS(out$summary$matrix, file.path(art_dir, fname))
    mk <- unique(out$markers[, c("factor_index", "gene", "score")])
    mk <- mk[!duplicated(mk[, c("factor_index", "gene")]), , drop = FALSE]
    markers[[key]] <- cbind(data.frame(fit_key = rep(key, nrow(mk))), mk)
    redundancy[[key]] <- data.frame(
      fit_key = key, max_offdiag_cosine = out$summary$max_offdiag,
      median_offdiag_cosine = out$summary$median_offdiag,
      n_factors_with_no_markers = out$summary$n_factors_with_no_markers)
    arts[[key]] <- data.frame(fit_key = key, kind = "redundancy",
                              path = fname)
  }
  list(pattern_markers = do.call(rbind, unname(markers)),
       fit_redundancy = do.call(rbind, unname(redundancy)),
       artifacts = do.call(rbind, unname(arts)))
}

#' ICASSO clusters for every ICA rank
#'
#' Pools every ok fit at a rank (seed sweep and bootstrap) and writes each
#' rank's dendrogram, its leaves labelled `"<fit_key>:<factor_index>"`.
#'
#' @param ingest The ICA ingest_method_fits() result.
#' @param results_root The results directory.
#' @return List (per rank) of `list(rank, clusters, membership)`, keyed by
#'   (fit_key, factor_index).
#' @keywords internal
#' @export
compute_method_icasso <- function(ingest, results_root) {
  f <- fits_with_loadings(ingest, results_root)
  art_dir <- artifacts_dir(results_root, ingest$dataset_id)
  out <- lapply(split(f, f$rank), function(r) {
    loadings <- lapply(r$loadings_file, readRDS)
    names(loadings) <- seq_len(nrow(r))
    res <- compute_icasso_clusters_from_loadings(loadings, r$rank[1])
    if (is.null(res)) return(NULL)
    key <- function(i) r$fit_key[as.integer(i)]
    fname <- sprintf("icasso_dendro_rank%d.rds", r$rank[1])
    saveRDS(list(merge = res$hclust$merge, height = res$hclust$height, order = res$hclust$order,
                 labels = paste0(key(res$comp_fit), ":", res$comp_factor)),
            file.path(art_dir, fname))
    cl <- res$clusters
    list(rank = r$rank[1],
         clusters = data.frame(cluster_id = cl$cluster_id, iq = cl$iq, n_members = cl$n_members,
                               centrotype_fit_key = key(cl$centrotype_fit_id),
                               centrotype_factor_index = cl$centrotype_factor_index,
                               dendro_file = fname),
         membership = data.frame(fit_key = key(res$membership$fit_id),
                                 factor_index = res$membership$factor_index,
                                 cluster_id = res$membership$cluster_id,
                                 intra_sim = res$membership$intra_sim))
  })
  Filter(Negate(is.null), out)
}
