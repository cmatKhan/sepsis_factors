# Cross-method agreement (see R/derive.R). Precomputes what the app computed
# live on its Compare page (`method_pairwise_agreement()` in
# sepsisfactorsapp/R/comparison_helpers.R): for two representative fits of
# different methods, the gene-set Jaccard of their top-N genes per factor
# matched one-to-one (Hungarian on |Jaccard|), averaged, and the adjusted Rand
# index of their sample clusterings (hierarchical, k = 4). PCA is one fit
# whose first n components stand for rank n, so each n is a variant.

#' The gene sets of one fit
#'
#' @return A list: `sets_at(n_top)` giving one integer vector of gene numbers
#'   per factor, `index` (factor index of each set) and `n_max`; `NULL` when
#'   the fit has nothing to compare.
#' @noRd
agreement_sets <- function(ctx, fit) {
  if (fit$method == "wgcna") {
    d <- DBI::dbGetQuery(
      ctx$con,
      "SELECT gene, module FROM gene_modules WHERE fit_id = ? AND module != 0",
      params = list(fit$fit_id)
    )
    if (nrow(d) == 0L) {
      return(NULL)
    }
    sets <- split(gene_idx_of(ctx, d$gene, "a WGCNA module"), d$module)
    return(
      list(sets_at = function(n_top) sets, index = as.integer(names(sets)), n_max = length(sets))
    )
  }
  L <- read_fit_artifact(ctx, fit$fit_id, "loadings")
  if (is.null(L)) {
    return(NULL)
  }
  gi <- gene_idx_of(ctx, rownames(L), "the loadings")
  keep <- min(max(AGREEMENT_TOP_N), nrow(L))
  ranked <- lapply(
    seq_len(ncol(L)),
    function(j) gi[order(abs(L[, j]), decreasing = TRUE)[seq_len(keep)]]
  )
  list(
    sets_at = function(n_top) lapply(ranked, function(r) r[seq_len(min(n_top, length(r)))]),
    index = seq_len(ncol(L)),
    n_max = ncol(L)
  )
}

#' Jaccard index between every set of `a` and every set of `b`
#'
#' @param a,b Lists of integer vectors (sets of gene numbers).
#' @return A matrix, `length(a)` x `length(b)`.
#' @noRd
jaccard_sets <- function(a, b) {
  u <- unique(c(unlist(a), unlist(b)))
  member <- function(sets) {
    m <- matrix(0, length(u), length(sets))
    m[cbind(match(unlist(sets), u), rep(seq_along(sets), lengths(sets)))] <- 1
    m
  }
  inter <- crossprod(member(a), member(b))
  union <- outer(lengths(a), lengths(b), "+") - inter
  inter / union
}

#' Cluster the samples of a score matrix into `k` groups
#'
#' @noRd
cluster_samples <- function(S, k = 4L) {
  stats::cutree(stats::hclust(stats::dist(S)), k = k)
}

#' The representative fits agreement is made for, in the order its rows are written
#'
#' By display order of the method, then rank, then fit id. The chunks of a
#' parallel build slice this order, so their rows follow one another as in a serial
#' build.
#'
#' @param fits A data frame with `method`, `rank`, `fit_id`, `status`,
#'   `representative`.
#' @noRd
agreement_fits <- function(fits) {
  reps <- fits[fits$status == "ok" & fits$representative == 1L & fits$method %in% names(
    SUPPORTED_METHOD_OUTPUTS
  ), , drop = FALSE]
  display_order <- vapply(reps$method, function(m) method_display(m)$order, 1L)
  reps[order(display_order, reps$rank, reps$fit_id), , drop = FALSE]
}

derive_method_agreement <- function(ctx) {
  con <- ctx$con
  id <- ctx$dataset_id
  DBI::dbExecute(con, "DELETE FROM method_agreement WHERE dataset_id = ?", params = list(id))
  reps <- agreement_fits(ctx$all_fits)
  if (nrow(reps) == 0L) {
    return(0L)
  }
  fits <- lapply(seq_len(nrow(reps)), function(i) {
    f <- as.list(reps[i, ])
    f$sets <- agreement_sets(ctx, f)
    f$in_scope <- f$fit_id %in% ctx$fits$fit_id
    f$scores <- if (
      reps$fit_id[i] %in% ctx$all_fit_files$fit_id[ctx$all_fit_files$kind == "scores"]
    ) {
      read_fit_artifact(ctx, f$fit_id, "scores")
    } else {
      NULL
    }
    if (!is.null(f$sets)) {
      k <- f$sets$n_max
      if (f$method == "pca") {
        mse <- DBI::dbGetQuery(
          con,
          "SELECT COUNT(*) AS n FROM fit_diag WHERE fit_id = ? AND name = 'mse_by_rank'",
          params = list(f$fit_id)
        )$n
        if (mse > 0L) k <- min(k, mse)
      }
      f$variants <- if (f$method == "pca") seq_len(k) else k
    }
    f
  })
  fits <- Filter(function(f) !is.null(f$sets), fits)
  # only PCA is cut to its first n components; the other methods use every
  # score column (WGCNA has eigengenes beyond its numbered modules)
  score_cols <- function(f, n) if (f$method == "pca") seq_len(n) else seq_len(ncol(f$scores))
  # the sample clustering of every (fit, n) pair is needed by many method pairs, so make it
  # once per fit here
  fits <- lapply(fits, function(f) {
    f$clusters <- if (is.null(f$scores)) {
      list()
    } else {
      stats::setNames(
        lapply(f$variants, function(n) {
          S <- f$scores[, score_cols(f, n), drop = FALSE]
          if (nrow(S) >= 4L) cluster_samples(S) else NA
        }),
        f$variants
      )
    }
    f
  })
  cluster_of <- function(f, n) f$clusters[[as.character(n)]]
  ari_of <- function(fa, na, fb, nb) {
    if (is.null(fa$scores) || is.null(fb$scores)) {
      return(c(NA_real_, NA_real_))
    }
    ra <- rownames(fa$scores)
    rb <- rownames(fb$scores)
    shared <- intersect(ra, rb)
    if (length(shared) < 4L) {
      return(c(NA_real_, length(shared)))
    }
    if (identical(ra, rb)) {
      ca <- cluster_of(fa, na)
      cb <- cluster_of(fb, nb)
    } else {
      ca <- cluster_samples(fa$scores[shared, score_cols(fa, na), drop = FALSE])
      cb <- cluster_samples(fb$scores[shared, score_cols(fb, nb), drop = FALSE])
    }
    c(mclust::adjustedRandIndex(ca, cb), length(shared))
  }

  total <- 0L
  for (i in seq_along(fits)) {
    for (j in seq_along(fits)) {
      fa <- fits[[i]]
      fb <- fits[[j]]
      if (!fa$in_scope || method_display(fa$method)$order >= method_display(fb$method)$order) next
      aris <- list()
      for (n_top in AGREEMENT_TOP_N) {
        J <- jaccard_sets(fa$sets$sets_at(n_top), fb$sets$sets_at(n_top))
        rows <- list()
        for (na in fa$variants) {
          for (nb in fb$variants) {
            jm <- J[seq_len(na), seq_len(nb), drop = FALSE]
            idx <- tryCatch(hungarian_match_abs(jm), error = function(e) NULL)
            if (is.null(idx)) next
            key <- paste(na, nb)
            if (is.null(aris[[key]])) aris[[key]] <- ari_of(fa, na, fb, nb)
            rows[[length(rows) + 1L]] <- list(
              na = na,
              nb = nb,
              jaccard = mean(jm[idx], na.rm = TRUE),
              ari = aris[[key]][1],
              shared = aris[[key]][2],
              matrix = encode_f32(jm),
              matches = encode_i32(
                c(fa$sets$index[seq_len(na)], fb$sets$index[seq_len(nb)], idx[, "a"], idx[, "b"])
              )
            )
          }
        }
        if (length(rows)) {
          col <- function(name, f = identity) f(lapply(rows, `[[`, name))
          total <- total + insert_columns(
            con,
            "method_agreement",
            list(
              dataset_id = id,
              fit_a = fa$fit_id,
              n_factors_a = col("na", unlist),
              fit_b = fb$fit_id,
              n_factors_b = col("nb", unlist),
              top_n = n_top,
              gene_jaccard = col("jaccard", unlist),
              sample_ari = col("ari", unlist),
              n_shared = as.integer(col("shared", unlist)),
              jaccard_matrix = col("matrix"),
              matches = col("matches")
            )
          )
        }
      }
    }
  }
  total
}
