# Precomputed comparisons: cross-method agreement of representative fits (table
# `method_agreement`) and the association of factor scores with sample metadata (table
# `factor_association`).

# The top-N values (genes per factor) the agreement was computed at for a dataset.
agreement_grid <- function(db, dataset_id) {
  db_query(
    db,
    "SELECT DISTINCT top_n FROM method_agreement WHERE dataset_id = ? ORDER BY top_n",
    params = list(dataset_id)
  )$top_n
}

# Labels of a fit's factors, named by factor index.
factor_labels <- function(db, fit_id) {
  f <- db_query(
    db, "SELECT factor_index, label FROM factors WHERE fit_id = ? ORDER BY factor_index",
    params = list(fit_id)
  )
  stats::setNames(
    ifelse(is.na(f$label), paste0("factor_", f$factor_index), f$label),
    f$factor_index
  )
}

# One stored agreement row for two fits (at n_a and n_b components) at `top_n`; NULL if none.
agreement_row <- function(db, fit_a, n_a, fit_b, n_b, top_n) {
  d <- db_query(
    db,
    "SELECT gene_jaccard, sample_ari, n_shared, jaccard_matrix, matches
     FROM method_agreement
     WHERE fit_a = ? AND n_factors_a = ? AND fit_b = ? AND n_factors_b = ? AND top_n = ?",
    params = list(fit_a, n_a, fit_b, n_b, top_n)
  )
  if (nrow(d) == 1) d else NULL
}

# The Jaccard matrix and the one-to-one matches of a stored row. `labels_a`/`labels_b`
# name the factors by factor index. Returns list(matrix, matches, mean) with `matches`
# holding a, b and jaccard (largest first), as the Compare page shows them.
decode_agreement <- function(row, n_a, n_b, labels_a, labels_b) {
  m <- decode_i32(row$matches[[1]])
  k <- min(n_a, n_b)
  index_a <- m[seq_len(n_a)]
  index_b <- m[n_a + seq_len(n_b)]
  match_a <- m[n_a + n_b + seq_len(k)]
  match_b <- m[n_a + n_b + k + seq_len(k)]
  jm <- matrix(decode_f32(row$jaccard_matrix[[1]]), n_a, n_b)
  dimnames(jm) <- list(
    unname(labels_a[as.character(index_a)]), unname(labels_b[as.character(index_b)])
  )
  matches <- data.frame(
    a = rownames(jm)[match_a], b = colnames(jm)[match_b], jaccard = jm[cbind(match_a, match_b)],
    stringsAsFactors = FALSE
  )
  list(
    matrix = jm, matches = matches[order(-matches$jaccard), ], mean = row$gene_jaccard
  )
}

# Method x method agreement of one dataset's optimal fits (`optimal` is optimal_fits()
# output, in display order, which is the order the stored pairs use). Returns
# list(methods, gene_jaccard, sample_ari, pairs): symmetric matrices with diagonal 1 and,
# for each pair that was stored, its decoded Jaccard matrix and matches under the key
# "<method a>|<method b>".
method_pairwise_agreement <- function(db, optimal, top_n = 50) {
  methods <- optimal$method
  n <- length(methods)
  gj <- sa <- matrix(NA_real_, n, n, dimnames = list(methods, methods))
  diag(gj) <- 1
  diag(sa) <- 1
  pairs <- list()
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i >= j) {
        next
      }
      row <- agreement_row(
        db, optimal$fit_id[i], optimal$n_factors[i], optimal$fit_id[j], optimal$n_factors[j], top_n
      )
      if (is.null(row)) {
        next
      }
      gj[i, j] <- gj[j, i] <- row$gene_jaccard
      sa[i, j] <- sa[j, i] <- row$sample_ari
      pairs[[paste(methods[i], methods[j], sep = "|")]] <- decode_agreement(
        row, optimal$n_factors[i], optimal$n_factors[j],
        factor_labels(db, optimal$fit_id[i]), factor_labels(db, optimal$fit_id[j])
      )
    }
  }
  list(methods = methods, gene_jaccard = gj, sample_ari = sa, pairs = pairs)
}

# Association of every score column with every metadata field, as stored: Spearman for
# numeric fields, Kruskal-Wallis for the rest. `n` keeps the first n factors; the stored
# BH adjustment is over all factors of the fit, so it is redone when some are dropped.
# Columns: component (the factor label), field, test, statistic, p_value, padj.
association_for_fit <- function(db, fit_id, n = NULL) {
  d <- db_query(
    db,
    "SELECT a.factor_index, x.label AS component, a.field, a.test, a.statistic, a.p_value,
            a.p_adj AS padj
     FROM factor_association a
     JOIN factors x ON x.fit_id = a.fit_id AND x.factor_index = a.factor_index
     WHERE a.fit_id = ? ORDER BY a.factor_index, a.field",
    params = list(fit_id)
  )
  if (!is.null(n) && !is.na(n) && any(d$factor_index > n)) {
    d <- d[d$factor_index <= n, , drop = FALSE]
    d$padj <- stats::p.adjust(d$p_value, method = "BH")
  }
  d[c("component", "field", "test", "statistic", "p_value", "padj")]
}
