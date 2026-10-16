# Derive steps that summarise a loaded dataset (see R/derive.R). They read the
# database tables load_dataset() filled, not the artifacts, except for the
# association scan, which needs the scores and the sample metadata.

#' Quality and stability of each representative fit
#'
#' Ports `method_curve()` of the app, one row per representative ok fit:
#' * PCA: `1 - mse_by_rank[n_factors] / val_sd^2` from `fit_diag`; stability 1
#'   (nested components are stable by construction).
#' * sPCA: `pev_sparse` from `fit_metrics`.
#' * WGCNA: the member-weighted mean kME of the non-grey modules; stability is
#'   the mean ARI against the other representative fits.
#' * Others: `1 - mse / val_sd^2`.
#' Except for WGCNA and PCA, stability is the mean over factors of
#' `factors.stability_<metric>`.
#'
#' @noRd
derive_fit_summary <- function(ctx) {
  con <- ctx$con
  id <- ctx$dataset_id
  DBI::dbExecute(
    con,
    "DELETE FROM fit_summary WHERE fit_id IN (SELECT fit_id FROM fits WHERE dataset_id = ?)",
    params = list(id)
  )
  all_reps <- DBI::dbGetQuery(
    con,
    "SELECT fit_id, method, mse, n_factors FROM fits
     WHERE dataset_id = ? AND status = 'ok' AND representative = 1",
    params = list(id)
  )
  fits <- all_reps[all_reps$fit_id %in% ctx$fits$fit_id, , drop = FALSE]
  if (nrow(fits) == 0L) {
    return(0L)
  }
  val_sd <- DBI::dbGetQuery(
    con,
    "SELECT val_sd FROM matrix_diagnostics WHERE dataset_id = ?",
    params = list(id)
  )$val_sd
  val_sd <- if (length(val_sd) == 1L && !is.na(val_sd)) val_sd else NA_real_
  in_ids <- paste(fits$fit_id, collapse = ",")
  stab <- DBI::dbGetQuery(
    con,
    sprintf(
      paste0(
        "SELECT fit_id, AVG(stability_cosine) AS cosine, AVG(stability_pearson) AS pearson,\n",
        "              AVG(stability_spearman) AS spearman FROM factors WHERE fit_id IN (%s) ",
        "GROUP BY fit_id"
      ),
      in_ids
    )
  )
  pev <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT fit_id, value FROM fit_metrics WHERE name = 'pev_sparse' AND fit_id IN (%s)",
      in_ids
    )
  )
  kme <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT fit_id, SUM(n * mean) / SUM(n) AS q FROM module_kme_summary
       WHERE grp = 'member' AND module != 0 AND fit_id IN (%s) GROUP BY fit_id",
      in_ids
    )
  )
  ari <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT fit_a, fit_b, ari FROM fit_pairs WHERE fit_a IN (%s) AND fit_b IN (%s)",
      paste(all_reps$fit_id, collapse = ","),
      paste(all_reps$fit_id, collapse = ",")
    )
  )
  ari_long <- rbind(
    data.frame(fit_id = ari$fit_a, ari = ari$ari),
    data.frame(fit_id = ari$fit_b, ari = ari$ari)
  )
  ari_mean <- if (nrow(ari_long)) {
    stats::aggregate(ari ~ fit_id, ari_long, mean)
  } else {
    data.frame(
      fit_id = integer(),
      ari = numeric()
    )
  }
  mse_by_rank <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT fit_id, idx, value FROM fit_diag WHERE name = 'mse_by_rank' AND fit_id IN (%s)",
      in_ids
    )
  )

  at <- function(df, col, fit_id) df[[col]][match(fit_id, df$fit_id)]
  out <- data.frame(
    fit_id = fits$fit_id,
    in_sample_r2 = NA_real_,
    stability_cosine = at(stab, "cosine", fits$fit_id),
    stability_pearson = at(stab, "pearson", fits$fit_id),
    stability_spearman = at(stab, "spearman", fits$fit_id)
  )
  for (i in seq_len(nrow(fits))) {
    fit_id <- fits$fit_id[i]
    switch(fits$method[i],
      pca = {
        m <- mse_by_rank[mse_by_rank$fit_id == fit_id, , drop = FALSE]
        k <- fits$n_factors[i]
        out$in_sample_r2[i] <- if (is.na(k) || !k %in% m$idx) {
          NA_real_
        } else {
          1 - m$value[m$idx == k] / val_sd^2
        }
        out[i, c("stability_cosine", "stability_pearson", "stability_spearman")] <- 1
      },
      spca = out$in_sample_r2[i] <- at(pev, "value", fit_id),
      wgcna = {
        out$in_sample_r2[i] <- at(kme, "q", fit_id)
        out[i, c("stability_cosine", "stability_pearson", "stability_spearman")] <- at(
          ari_mean,
          "ari",
          fit_id
        )
      },
      out$in_sample_r2[i] <- 1 - fits$mse[i] / val_sd^2
    )
  }
  DBI::dbAppendTable(con, "fit_summary", out)
  nrow(out)
}

#' Association of each factor's scores with each sample-metadata field
#'
#' For the representative fits with scores (the fits the app looks at). For
#' PCA the Benjamini-Hochberg `p_adj` is over the whole fit; the app's nested
#' ranks use the first components, so a reader recomputes it from `p_value`.
#'
#' @noRd
derive_factor_association <- function(ctx) {
  con <- ctx$con
  id <- ctx$dataset_id
  DBI::dbExecute(
    con,
    "DELETE FROM factor_association WHERE fit_id IN (SELECT fit_id FROM fits WHERE dataset_id = ?)",
    params = list(id)
  )
  meta <- read_dataset_artifact(ctx, "sample_metadata")
  id_col <- ctx$dataset$sample_id_col
  if (is.null(meta) || is.na(id_col) || !id_col %in% names(meta)) {
    return(0L)
  }
  meta <- as.data.frame(meta)
  names(meta)[names(meta) == id_col] <- "sample_id"
  meta$sample_id <- as.character(meta$sample_id)
  reps <- ctx$fits[ctx$fits$status == "ok" & ctx$fits$representative == 1L, , drop = FALSE]
  with_scores <- ctx$fit_files$fit_id[ctx$fit_files$kind == "scores"]
  reps <- reps[reps$fit_id %in% with_scores, , drop = FALSE]
  n <- 0L
  for (fit_id in reps$fit_id) {
    S <- read_fit_artifact(ctx, fit_id, "scores")
    fac <- DBI::dbGetQuery(
      con,
      "SELECT factor_index, label FROM factors WHERE fit_id = ? ORDER BY factor_index",
      params = list(fit_id)
    )
    if (reps$method[reps$fit_id == fit_id] == "wgcna") {
      S <- S[, intersect(fac$label, colnames(S)), drop = FALSE]
    } else {
      S <- S[, seq_len(min(nrow(fac), ncol(S))), drop = FALSE]
      colnames(S) <- fac$label[seq_len(ncol(S))]
    }
    sc <- association_scan(S, meta)
    if (nrow(sc) == 0L) {
      next
    }
    rows <- data.frame(
      fit_id = fit_id,
      factor_index = fac$factor_index[match(sc$component, fac$label)],
      field = sc$field,
      test = sc$test,
      statistic = sc$statistic,
      p_value = sc$p_value,
      p_adj = sc$padj
    )
    DBI::dbAppendTable(con, "factor_association", rows)
    n <- n + nrow(rows)
  }
  n
}

# TEMPORARY copy of association_scan() of sepsisfactorsapp (R/data_helpers.R),
# which computed it live: Spearman for numeric fields, Kruskal-Wallis for the
# rest, BH over the grid. Removed with the derive step.
association_scan <- function(scores, meta, id_col = "sample_id") {
  empty <- data.frame(
    component = character(0), field = character(0), test = character(0),
    statistic = numeric(0), p_value = numeric(0), padj = numeric(0)
  )
  if (is.null(scores) || is.null(meta) || !id_col %in% names(meta)) {
    return(empty)
  }
  shared <- intersect(rownames(scores), meta[[id_col]])
  if (length(shared) < 3) {
    return(empty)
  }
  scores <- scores[shared, , drop = FALSE]
  meta <- meta[match(shared, meta[[id_col]]), , drop = FALSE]
  comps <- colnames(scores) %||% as.character(seq_len(ncol(scores)))
  rows <- list()
  for (field in setdiff(names(meta), id_col)) {
    v <- meta[[field]]
    if ((is.numeric(v) || is.integer(v)) && !is.factor(v)) {
      ok <- !is.na(v)
      if (sum(ok) < 3 || length(unique(v[ok])) < 2) next
      r <- suppressWarnings(
        stats::cor(
          scores[ok, , drop = FALSE],
          v[ok],
          method = "spearman",
          use = "pairwise.complete.obs"
        )
      )[, 1]
      n <- colSums(!is.na(scores[ok, , drop = FALSE]))
      keep <- !is.na(r) & n >= 3
      if (!any(keep)) next
      t <- r[keep] * sqrt((n[keep] - 2) / pmax(1 - r[keep]^2, 1e-12))
      rows[[length(rows) + 1]] <- data.frame(
        component = comps[keep], field = field, test = "spearman",
        statistic = unname(r[keep]), p_value = 2 * stats::pt(-abs(t), n[keep] - 2)
      )
    } else {
      f <- droplevels(as.factor(v))
      for (j in seq_along(comps)) {
        ok <- !is.na(f) & !is.na(scores[, j])
        if (sum(ok) < 3 || nlevels(droplevels(f[ok])) < 2) next
        kt <- tryCatch(
          stats::kruskal.test(scores[ok, j] ~ droplevels(f[ok])),
          error = function(e) NULL
        )
        if (is.null(kt)) next
        rows[[length(rows) + 1]] <- data.frame(
          component = comps[j], field = field, test = "kruskal",
          statistic = unname(kt$statistic), p_value = kt$p.value
        )
      }
    }
  }
  if (length(rows) == 0) {
    return(empty)
  }
  out <- do.call(rbind, rows)
  out$padj <- stats::p.adjust(out$p_value, method = "BH")
  out
}
