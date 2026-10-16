# The results output: the tables and artifacts this package hands to
# sepsisfactorsdb. One Parquet file per database table, with the table's
# columns except that surrogate ids are replaced by natural keys (fit_key,
# factor_index, ...); sepsisfactorsdb documents the specification and loads
# it. Layout under the results root:
#   manifest.json
#   datasets/<id>/tables/<table>.parquet
#   datasets/<id>/artifacts/<file>.rds
#   cross_dataset/tables/<table>.parquet
#   cross_dataset/artifacts/<file>.rds
# Artifacts are written by the targets that compute them (the extraction,
# diagnostics and projection targets); the writers here write the tables, which
# name the artifacts by file name.

#' The version of the results-output format this package writes
#'
#' Written to `manifest.json` as `results_output_version`; `sepsisfactorsdb`
#' refuses a results output of a version it does not load. Each method's
#' `method_output_version` is in its output spec.
#'
#' @format An integer.
#' @export
RESULTS_OUTPUT_VERSION <- 1L

#' A dataset's artifact directory in the results output
#'
#' @param results_root The results directory.
#' @param dataset_id Dataset id.
#' @return `<results_root>/datasets/<dataset_id>/artifacts`.
#' @noRd
artifacts_dir <- function(results_root, dataset_id) {
  file.path(results_root, "datasets", dataset_id, "artifacts")
}

#' A dataset's table directory in the results output
#'
#' @inheritParams artifacts_dir
#' @return `<results_root>/datasets/<dataset_id>/tables`.
#' @noRd
tables_dir <- function(results_root, dataset_id) {
  file.path(results_root, "datasets", dataset_id, "tables")
}

#' The cross-dataset directories of the results output
#'
#' @param results_root The results directory.
#' @return `<results_root>/cross_dataset/tables` and `.../artifacts`.
#' @noRd
cross_tables_dir <- function(results_root) file.path(results_root, "cross_dataset", "tables")

#' @rdname cross_tables_dir
#' @noRd
cross_artifacts_dir <- function(results_root) file.path(results_root, "cross_dataset", "artifacts")

#' Write results tables as Parquet files
#'
#' Empty and `NULL` tables are not written. Factors become character.
#'
#' @param tables Named list of data frames (names are table names).
#' @param dir Directory to write into (created).
#' @return The written paths.
#' @noRd
write_tables <- function(tables, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tables <- Filter(function(df) !is.null(df) && nrow(df) > 0, tables)
  paths <- file.path(dir, paste0(names(tables), ".parquet"))
  for (i in seq_along(tables)) {
    df <- as.data.frame(tables[[i]], stringsAsFactors = FALSE)
    rownames(df) <- NULL
    for (col in names(df)) if (is.factor(df[[col]])) df[[col]] <- as.character(df[[col]])
    arrow::write_parquet(df, paths[i])
  }
  paths
}

#' Bind one component across methods
#'
#' @param methods Named list (by method) of the writer's per-method inputs.
#' @param getter `function(parts, method)` returning a data frame or `NULL`.
#' @return The bound data frame, or `NULL`.
#' @noRd
rbind_parts <- function(methods, getter) {
  rows <- Filter(Negate(is.null), lapply(names(methods), function(m) getter(methods[[m]], m)))
  if (length(rows)) do.call(rbind, unname(rows)) else NULL
}

#' Rename columns
#'
#' @param df A data frame, or `NULL`.
#' @param from,to Old and new names.
#' @return `df` with the columns renamed.
#' @noRd
rename_cols <- function(df, from, to) {
  if (is.null(df)) {
    return(NULL)
  }
  names(df)[match(from, names(df))] <- to
  df
}

#' The pair-stability tables of one method
#'
#' @param pr A compute_factor_pairs() or compute_wgcna_pairs() result.
#' @param dataset_id Dataset id.
#' @param method Method name.
#' @return Named list of `fit_pairs`, `factor_matches`, `match_scores`,
#'   `fit_pair_null` and `similarity_histograms` data frames (any may be `NULL`).
#' @noRd
pair_tables <- function(pr, dataset_id, method) {
  if (is.null(pr) || is.null(pr$fit_pairs)) {
    return(list())
  }
  matches <- pr$matches
  scores <- NULL
  if (!is.null(matches) && nrow(matches)) {
    # `match` numbers a method's matches; it is only a join key here.
    matches$factor_a_index <- matches$factor_a
    matches$factor_b_index <- matches$factor_b
    key <- match(pr$scores$match, matches$match)
    scores <- data.frame(
      fit_a_key = matches$fit_a[key],
      fit_b_key = matches$fit_b[key],
      factor_a_index = matches$factor_a_index[key],
      pr$scores[, c("metric", "value", "runner_up", "margin")]
    )
    matches <- data.frame(
      fit_a_key = matches$fit_a,
      fit_b_key = matches$fit_b,
      factor_a_index = matches$factor_a_index,
      factor_b_index = matches$factor_b_index
    )
  }
  list(
    fit_pairs = rename_cols(pr$fit_pairs, c("fit_a", "fit_b"), c("fit_a_key", "fit_b_key")),
    factor_matches = matches,
    match_scores = scores,
    fit_pair_null = rename_cols(pr$null, c("fit_a", "fit_b"), c("fit_a_key", "fit_b_key")),
    similarity_histograms = if (!is.null(pr$histograms)) {
      cbind(data.frame(dataset_id = dataset_id, method = method), pr$histograms)
    }
  )
}

#' Write one dataset's tables (the `results_<dataset>` target)
#'
#' Writes everything the results output holds about one dataset, as Parquet
#' tables under `datasets/<id>/tables/`, replacing what an earlier run wrote.
#' The artifacts they name were written by the targets that compute them.
#'
#' @param results_root The results directory.
#' @param meta The resolved dataset config.
#' @param dataset_files Paths of the dataset's artifact files (matrix.rds,
#'   sample_metadata.rds, ...); the kind is the file name.
#' @param methods Named list (by method) of `list(ingest, pairs,
#'   representatives, redundancy, icasso, selection, wgcna_fits,
#'   factor_variance)`.
#' @param method_table method_table() output.
#' @param enrichment enrichment_rows() output.
#' @param matrix_diagnostics matrix_diagnostics_row() output.
#' @param wgcna wgcna_dataset_diagnostics() output, or `NULL`.
#' @param drivers pattern_driver_rows() output.
#' @param variance variance_inputs() output, or `NULL` (no design for the
#'   dataset or its parent).
#' @return The written table paths.
#' @keywords internal
#' @export
write_dataset_results <- function(
  results_root,
  meta,
  dataset_files,
  methods,
  method_table,
  enrichment = NULL,
  matrix_diagnostics = NULL,
  wgcna = NULL,
  drivers = NULL,
  variance = NULL
) {
  ds <- meta$dataset
  id <- ds$id
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  unlink(tables_dir(results_root, id), recursive = TRUE)
  tables <- list()

  tables$datasets <- data.frame(
    dataset_id = id,
    description = trimws(ds$description %||% NA_character_),
    n_samples = as.integer(matrix_diagnostics$n_samples),
    n_genes = as.integer(matrix_diagnostics$n_genes),
    sample_id_col = ds$sample_id_col %||% NA_character_,
    feature_id_col = ds$feature_id_col %||% NA_character_,
    ensembl_col = ds$ensembl_col %||% NA_character_,
    symbol_col = ds$symbol_col %||% NA_character_,
    subject_id_col = ds$subject_id_col %||% NA_character_,
    sample_metadata_source = ds$sample_metadata_path %||% NA_character_,
    feature_metadata_source = ds$feature_metadata_path %||% NA_character_,
    written_at = now
  )
  tables$methods <- method_table[method_table$method %in% names(methods), , drop = FALSE]

  # ---- fits and their long tables ----
  tables$fits <- rbind_parts(methods, function(p, m) {
    f <- p$ingest$fits
    f$dataset_id <- id
    f$method <- m
    f$representative <- as.integer(f$fit_key %in% p$representatives)
    f$written_at <- now
    f[, c("fit_key", "dataset_id", "method", "rank", "seed", "bootstrap", "representative", "mse",
          "n_factors", "status", "error", "written_at")]
  })
  tables$fit_params <- rbind_parts(methods, function(p, m) p$ingest$params)
  tables$fit_metrics <- rbind_parts(methods, function(p, m) {
    rbind(p$ingest$metrics, selection_metric_rows(p$ingest, p$selection))
  })
  tables$fit_artifacts <- rbind_parts(methods, function(p, m) {
    rbind(
      p$ingest$artifacts, p$redundancy$artifacts,
      do.call(rbind, lapply(p$wgcna_fits, `[[`, "artifacts"))
    )
  })
  tables$factors <- rbind_parts(methods, function(p, m) {
    f <- p$ingest$factors
    if (is.null(f)) {
      return(NULL)
    }
    st <- p$pairs$stability
    hit <- if (is.null(st)) {
      rep(NA_integer_, nrow(f))
    } else {
      match(paste(f$fit_key, f$factor_index), paste(st$fit_key, st$factor_index))
    }
    f$stability_cosine <- if (is.null(st)) NA_real_ else st$cosine[hit]
    f$stability_pearson <- if (is.null(st)) NA_real_ else st$pearson[hit]
    f$stability_spearman <- if (is.null(st)) NA_real_ else st$spearman[hit]
    f
  })

  # ---- pair stability ----
  pairs <- lapply(names(methods), function(m) pair_tables(methods[[m]]$pairs, id, m))
  for (table in c("fit_pairs", "factor_matches", "match_scores", "fit_pair_null", "similarity_histograms")) {
    tables[[table]] <- do.call(rbind, lapply(pairs, `[[`, table))
  }

  # ---- WGCNA per-fit kME, redundancy, ICASSO ----
  wgcna_fits <- lapply(methods, function(p) Filter(Negate(is.null), p$wgcna_fits))
  bind_wgcna <- function(part) {
    rows <- lapply(wgcna_fits, function(wf) do.call(rbind, lapply(wf, `[[`, part)))
    do.call(rbind, unname(Filter(Negate(is.null), rows)))
  }
  tables$gene_modules <- bind_wgcna("gene_modules")
  tables$module_kme_summary <- bind_wgcna("summary")
  tables$module_kme_histograms <- bind_wgcna("histograms")
  tables$fit_redundancy <- rbind_parts(methods, function(p, m) p$redundancy$fit_redundancy)
  tables$pattern_drivers <- drivers
  icasso <- unlist(lapply(methods, `[[`, "icasso"), recursive = FALSE)
  if (length(icasso)) {
    tables$icasso_clusters <- do.call(rbind, lapply(icasso, function(ic) {
      cbind(data.frame(dataset_id = id, rank = ic$rank), ic$clusters)
    }))
    tables$icasso_membership <- do.call(rbind, lapply(icasso, function(ic) {
      cbind(ic$membership, data.frame(dataset_id = id, rank = ic$rank))
    }))
  }

  # ---- enrichment ----
  if (!is.null(enrichment$queries) && nrow(enrichment$queries)) {
    tables$enrichment_queries <- cbind(enrichment$queries, data.frame(queried_at = now))
    # The leading-edge and overlap genes are in the enrichment_genes artifact.
    tables$enrichment_results <- enrichment$results[, setdiff(names(enrichment$results), "genes")]
  }

  # ---- score-level variance partition and the repeated-measures tables ----
  fvr <- factor_variance_rows(methods, id)
  if (!is.null(fvr)) {
    tables$factor_variance_models <- fvr$models
    tables$factor_variance <- fvr$rows
  }
  if (!is.null(variance)) {
    rows <- variance_db_rows(variance, now)
    for (table in names(rows)) tables[[table]] <- rows[[table]]
  }

  # ---- dataset-level ----
  if (length(dataset_files)) {
    tables$dataset_artifacts <- data.frame(
      dataset_id = id,
      kind = sub("\\.rds$", "", basename(dataset_files)),
      path = basename(dataset_files)
    )
  }
  if (!is.null(matrix_diagnostics)) {
    tables$matrix_diagnostics <- cbind(
      data.frame(dataset_id = id),
      matrix_diagnostics[, setdiff(names(matrix_diagnostics), c("n_genes", "n_samples"))],
      data.frame(computed_at = now)
    )
  }
  if (!is.null(wgcna)) {
    if (!is.null(wgcna$sft)) tables$wgcna_sft <- cbind(data.frame(dataset_id = id), wgcna$sft)
    if (!is.null(wgcna$significance)) {
      tables$gene_significance <- cbind(data.frame(dataset_id = id), wgcna$significance)
    }
  }
  write_tables(tables, tables_dir(results_root, id))
}

#' Write the cross-dataset tables (the `results_cross_dataset` target)
#'
#' Projections and gene-space agreement link two datasets' fits, so they are
#' written after every dataset's tables. Projection results no longer in the
#' tables are removed from `cross_dataset/artifacts/`.
#'
#' @param results_root The results directory.
#' @param written The `results_<dataset>` values (a dependency only).
#' @param projections Flat list of projection_rows() data frames.
#' @param gene_space List of gene_space_rows() data frames.
#' @param variance_explore combine_variance_explore() output, or `NULL` (its
#'   `time_gene_recurrence` becomes that table).
#' @param projection_scores List of projection_scores() data frames, joined to
#'   the projections by source fit and target dataset.
#' @return The written table paths.
#' @keywords internal
#' @export
write_cross_results <- function(
  results_root,
  written,
  projections = list(),
  gene_space = list(),
  variance_explore = NULL,
  projection_scores = list()
) {
  unlink(cross_tables_dir(results_root), recursive = TRUE)
  dir.create(cross_artifacts_dir(results_root), recursive = TRUE, showWarnings = FALSE)
  bind <- function(x) do.call(rbind, Filter(Negate(is.null), x))
  pr <- bind(projections)
  gs <- bind(gene_space)
  sc <- bind(projection_scores)
  if (!is.null(pr) && nrow(pr)) {
    hit <- if (is.null(sc)) {
      NA_integer_
    } else {
      match(paste(pr$source_fit_key, pr$target_dataset_id), paste(sc$source_fit_key, sc$target_dataset_id))
    }
    for (col in c("n_components", "transfer_r2", "native_r2", "relative_r2", "null_r2")) {
      pr[[col]] <- if (is.null(sc)) NA else sc[[col]][hit]
    }
    stale <- setdiff(list.files(cross_artifacts_dir(results_root)), pr$path)
    unlink(file.path(cross_artifacts_dir(results_root), stale))
  }
  write_tables(
    list(
      projections = pr,
      gene_space_agreement = gs,
      time_gene_recurrence = variance_explore$time_gene_recurrence
    ),
    cross_tables_dir(results_root)
  )
}

#' Write the manifest (the `results_manifest` target)
#'
#' Lists the datasets with tables in the results output and, for every method
#' present in them, its `method_output_version`.
#'
#' @param results_root The results directory.
#' @param dataset_ids The pipeline's dataset ids.
#' @param cross The `results_cross_dataset` value (a dependency only).
#' @return The manifest's path.
#' @keywords internal
#' @export
write_results_manifest <- function(results_root, dataset_ids, cross = NULL) {
  have <- dataset_ids[file.exists(file.path(tables_dir(results_root, dataset_ids), "datasets.parquet"))]
  present <- unique(unlist(lapply(have, function(id) {
    arrow::read_parquet(file.path(tables_dir(results_root, id), "methods.parquet"))$method
  })))
  manifest <- method_manifest()
  methods <- stats::setNames(
    lapply(present, function(m) list(method_output_version = manifest[[m]]$ingest$method_output_version)),
    present
  )
  path <- file.path(results_root, "manifest.json")
  dir.create(results_root, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    list(
      results_output_version = RESULTS_OUTPUT_VERSION,
      writer = list(package = "sepsisfactors", version = as.character(utils::packageVersion("sepsisfactors"))),
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      datasets = have,
      methods = methods
    ),
    path,
    auto_unbox = TRUE,
    pretty = TRUE
  )
  path
}
