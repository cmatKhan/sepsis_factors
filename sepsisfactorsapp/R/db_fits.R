# Queries about fits: quality/stability curves, optimal fits, pair stability, per-method
# detail, factors, WGCNA modules and enrichment. Connection handle first.

# Metrics with a per-factor stability in the database.
STABILITY_METRICS <- c("cosine", "pearson", "spearman")
# Metrics of the matched similarities (WGCNA modules are matched by Jaccard).
MATCH_METRICS <- c(STABILITY_METRICS, "jaccard")

check_metric <- function(metric, allowed = STABILITY_METRICS) {
  if (!metric %in% allowed) {
    cli::cli_abort("{.arg metric} must be one of {.val {allowed}}, not {.val {metric}}.")
  }
  metric
}

# The projections the app uses: the variant of each type that the pipeline computes
# (a within-dataset projection without an intercept, a cross-dataset one with it). A
# database holding both variants of a pair therefore never mixes them.
PROJECTION_VARIANT_SQL <- paste(
  "((projection_type = 'within_dataset' AND include_intercept = 0) OR",
  "(projection_type = 'cross_dataset' AND include_intercept = 1))"
)

## ---- curves and the optimal fit --------------------------------------------------------

empty_curve <- function() {
  data.frame(
    method = character(0), rank_label = character(0), rank_key = numeric(0),
    fit_id = integer(0), n_factors = integer(0), in_sample_r2 = numeric(0),
    stability = numeric(0), stringsAsFactors = FALSE
  )
}

# One method's quality/stability curve over its hyperparameter, one row per
# representative fit, read from `fit_summary`. `rank_key` is the rank, or the
# soft-threshold power where the fit has no rank (WGCNA); `rank_label` names which.
# Columns: in_sample_r2 and stability, defined per method as in fit_summary (comparable
# within a method, not across methods). An empty frame when the method has no ok fits.
method_curve <- function(db, dataset_id, method, metric = "cosine") {
  metric <- check_metric(metric)
  # PCA is one fit at the largest rank whose first n components stand for rank n, so its
  # curve over n comes from the per-rank errors in fit_diag, not from fit_summary.
  if (method == "pca") {
    return(pca_curve(db, dataset_id))
  }
  f <- db_query(
    db,
    "SELECT v.fit_id, v.rank, v.power, v.n_factors, s.in_sample_r2, s.stability_cosine,
            s.stability_pearson, s.stability_spearman
     FROM v_fits v JOIN fit_summary s ON s.fit_id = v.fit_id
     WHERE v.dataset_id = ? AND v.method = ? AND v.status = 'ok' AND v.representative = 1",
    params = list(dataset_id, method)
  )
  if (nrow(f) == 0) {
    return(empty_curve())
  }
  by_power <- is.na(f$rank)
  key <- ifelse(by_power, f$power, f$rank)
  ord <- order(key)
  f <- f[ord, ]
  by_power <- by_power[ord]
  key <- key[ord]
  data.frame(
    method = method,
    rank_label = paste(ifelse(by_power, "power", "rank"), key),
    rank_key = key, fit_id = f$fit_id, n_factors = f$n_factors,
    in_sample_r2 = f$in_sample_r2, stability = f[[paste0("stability_", metric)]],
    stringsAsFactors = FALSE
  )
}

# PCA's curve over n components: 1 - mse / val_sd^2 for each n; stability 1, because
# nested components are stable by construction.
pca_curve <- function(db, dataset_id) {
  f <- db_query(
    db,
    "SELECT fit_id FROM fits
     WHERE dataset_id = ? AND method = 'pca' AND status = 'ok' ORDER BY rank DESC LIMIT 1",
    params = list(dataset_id)
  )
  if (nrow(f) == 0) {
    return(empty_curve())
  }
  mse <- db_query(
    db,
    "SELECT idx, value FROM fit_diag WHERE fit_id = ? AND name = 'mse_by_rank' ORDER BY idx",
    params = list(f$fit_id)
  )
  if (nrow(mse) == 0) {
    return(empty_curve())
  }
  data.frame(
    method = "pca", rank_label = paste("rank", mse$idx), rank_key = mse$idx, fit_id = f$fit_id,
    n_factors = mse$idx, in_sample_r2 = 1 - mse$value / dataset_val_sd(db, dataset_id)^2,
    stability = 1, stringsAsFactors = FALSE
  )
}

dataset_val_sd <- function(db, dataset_id) {
  v <- db_query(
    db, "SELECT val_sd FROM matrix_diagnostics WHERE dataset_id = ?",
    params = list(dataset_id)
  )$val_sd
  if (length(v) == 1 && !is.na(v)) v else NA_real_
}

# Curves for every present method of one dataset, stacked.
dataset_curves <- function(db, dataset_id, methods, metric = "cosine") {
  out <- lapply(methods, function(m) method_curve(db, dataset_id, m, metric))
  do.call(rbind, c(list(empty_curve()), out))
}

# The optimal row of one method's curve.
#   "stable": among hyperparameters whose stability is at least `threshold`, the one
#             with the highest in-sample quality; if none qualifies, the most stable
#             one. Quality rises with rank, so this is the largest rank that is still
#             reproducible across seeds.
#   "max_r2": the highest in-sample quality, ignoring stability.
# Returns a one-row data.frame, or NULL for an empty curve.
pick_optimal <- function(curve, rule = c("stable", "max_r2"), threshold = 0.9) {
  rule <- match.arg(rule)
  d <- curve[!is.na(curve$in_sample_r2), , drop = FALSE]
  if (nrow(d) == 0) {
    return(NULL)
  }
  if (rule == "max_r2") {
    return(d[which.max(d$in_sample_r2), , drop = FALSE])
  }
  ok <- !is.na(d$stability) & d$stability >= threshold
  if (any(ok)) {
    return(d[ok, , drop = FALSE][which.max(d$in_sample_r2[ok]), , drop = FALSE])
  }
  if (all(is.na(d$stability))) {
    return(d[which.max(d$in_sample_r2), , drop = FALSE])
  }
  d[which.max(d$stability), , drop = FALSE]
}

# The optimal row for every method in `curves` (dataset_curves() output), in the
# display order of `info`.
optimal_fits <- function(curves, info, rule = "stable", threshold = 0.9) {
  rows <- lapply(split(curves, curves$method), pick_optimal, rule = rule, threshold = threshold)
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) {
    return(empty_curve())
  }
  rownames(out) <- NULL
  out[order(match(out$method, info$method)), ]
}

# Median out-of-sample score (relative transfer R2) of each method's fits, split by
# within-/cross-dataset targets, over every projection whose source fit belongs to this
# dataset and method.
oos_r2_by_method <- function(db, dataset_id) {
  d <- db_query(
    db,
    paste(
      "SELECT method, projection_type, relative_r2 FROM v_projections",
      "WHERE source_dataset_id = ? AND", PROJECTION_VARIANT_SQL
    ),
    params = list(dataset_id)
  )
  if (nrow(d) == 0) {
    return(data.frame(
      method = character(0), oos_r2_within = numeric(0), oos_r2_cross = numeric(0)
    ))
  }
  agg <- stats::aggregate(relative_r2 ~ method + projection_type, d, stats::median)
  methods <- unique(agg$method)
  pick <- function(type) {
    a <- agg[agg$projection_type == type, ]
    a$relative_r2[match(methods, a$method)]
  }
  data.frame(
    method = methods, oos_r2_within = pick("within_dataset"), oos_r2_cross = pick("cross_dataset")
  )
}

## ---- cross-dataset similarity --------------------------------------------------------------

empty_similarity <- function() {
  data.frame(source = character(0), target = character(0), value = numeric(0), n = integer(0))
}

# Long dataset x dataset similarity for one method.
#   "r2":         median relative transfer R2 of source-dataset fits on the target
#                 dataset's samples.
#   "gene_space": mean |diagonal| of the loadings similarity between the two datasets'
#                 representative fits, on shared genes.
# Columns: source, target, value, n (projections or matched genes).
cross_dataset_similarity <- function(db, method, kind = c("r2", "gene_space")) {
  kind <- match.arg(kind)
  if (kind == "r2") {
    d <- db_query(
      db,
      paste(
        "SELECT source_dataset_id AS source, target_dataset_id AS target,",
        "relative_r2 AS value FROM v_projections",
        "WHERE method = ? AND relative_r2 IS NOT NULL AND", PROJECTION_VARIANT_SQL
      ),
      params = list(method)
    )
    if (nrow(d) == 0) {
      return(empty_similarity())
    }
    agg <- stats::aggregate(value ~ source + target, d, stats::median)
    agg$n <- as.integer(stats::aggregate(value ~ source + target, d, length)$value)
    return(agg)
  }
  d <- db_query(
    db,
    "SELECT a.dataset_id AS source, b.dataset_id AS target, g.mean_abs_diagonal AS value,
            g.n_genes_matched AS n
     FROM gene_space_agreement g
     JOIN fits a ON a.fit_id = g.source_fit_id JOIN fits b ON b.fit_id = g.target_fit_id
     WHERE a.method = ?",
    params = list(method)
  )
  if (nrow(d) == 0) {
    return(empty_similarity())
  }
  agg <- stats::aggregate(value ~ source + target, d, mean)
  agg$n <- stats::aggregate(n ~ source + target, d, max)$n
  agg
}

# Square, symmetrised dataset x dataset matrix from cross_dataset_similarity() (the two
# directions are averaged; missing pairs stay NA), with rows ordered by hierarchical
# clustering (NA treated as 0 for the ordering only).
similarity_matrix <- function(long) {
  if (nrow(long) == 0) {
    return(NULL)
  }
  ds <- sort(unique(c(long$source, long$target)))
  m <- matrix(NA_real_, length(ds), length(ds), dimnames = list(ds, ds))
  m[cbind(match(long$source, ds), match(long$target, ds))] <- long$value
  sym <- (m + t(m)) / 2
  # one direction only: keep it
  sym[is.na(sym)] <- pmax(m, t(m), na.rm = TRUE)[is.na(sym)]
  diag(sym) <- NA
  if (length(ds) > 2) {
    z <- sym
    z[is.na(z)] <- 0
    ord <- stats::hclust(stats::dist(z))$order
    sym <- sym[ord, ord]
  }
  sym
}

## ---- explore fits: one method on one dataset -------------------------------------------------

# Every fit of a method on a dataset (ok or failed), with hyperparameters.
method_fits <- function(db, dataset_id, method) {
  db_query(
    db,
    "SELECT fit_id, rank, seed, bootstrap, representative, mse, n_factors, status, power, para
     FROM v_fits WHERE dataset_id = ? AND method = ?",
    params = list(dataset_id, method)
  )
}

# Matched |similarity| of every factor across same-rank fit pairs (the values
# `factors.stability_*` summarises). `kind` says whether the pair is two seeds or
# involves a bootstrap resample (ICA).
same_rank_matches <- function(db, dataset_id, method, metric = "cosine") {
  db_query(
    db,
    "SELECT rank, fit_pair_id, value, kind FROM v_same_rank_matches
     WHERE dataset_id = ? AND method = ? AND metric = ?",
    params = list(dataset_id, method, check_metric(metric, MATCH_METRICS))
  )
}

# Mean 95th percentile of the unmatched-cell |similarity| per rank: how large a match has
# to be to beat chance for that pair of fits.
same_rank_null <- function(db, dataset_id, method, metric = "cosine") {
  db_query(
    db,
    "SELECT fa.rank AS rank, AVG(n.p95) AS p95
     FROM fit_pairs p JOIN fits fa ON fa.fit_id = p.fit_a
     JOIN fit_pair_null n ON n.fit_pair_id = p.fit_pair_id AND n.metric = ?
     WHERE fa.dataset_id = ? AND fa.method = ? AND p.same_rank = 1 GROUP BY fa.rank",
    params = list(check_metric(metric, MATCH_METRICS), dataset_id, method)
  )
}

# Seed x seed matrix of mean matched |similarity| at one rank (non-bootstrap fits).
seed_pair_matrix <- function(db, dataset_id, method, rank, metric = "cosine") {
  d <- db_query(
    db,
    "SELECT fa.seed AS seed_a, fb.seed AS seed_b, AVG(ABS(s.value)) AS value
     FROM fit_pairs p
     JOIN fits fa ON fa.fit_id = p.fit_a JOIN fits fb ON fb.fit_id = p.fit_b
     JOIN factor_matches m ON m.fit_pair_id = p.fit_pair_id
     JOIN match_scores s ON s.match_id = m.match_id AND s.metric = ?
     WHERE fa.dataset_id = ? AND fa.method = ? AND fa.rank = ? AND p.same_rank = 1
       AND COALESCE(fa.bootstrap, 0) = 0 AND COALESCE(fb.bootstrap, 0) = 0
     GROUP BY p.fit_pair_id",
    params = list(check_metric(metric, MATCH_METRICS), dataset_id, method, rank)
  )
  if (nrow(d) == 0) {
    return(NULL)
  }
  seeds <- as.character(sort(unique(c(d$seed_a, d$seed_b))))
  m <- matrix(NA_real_, length(seeds), length(seeds), dimnames = list(seeds, seeds))
  m[cbind(match(d$seed_a, seeds), match(d$seed_b, seeds))] <- d$value
  m[cbind(match(d$seed_b, seeds), match(d$seed_a, seeds))] <- d$value
  m
}

# Per-factor stability of one fit.
fit_factor_stability <- function(db, fit_id, metric = "cosine") {
  d <- db_query(
    db,
    "SELECT factor_index, label, stability_cosine, stability_pearson, stability_spearman, n_genes
     FROM factors WHERE fit_id = ? ORDER BY factor_index",
    params = list(fit_id)
  )
  d$stability <- d[[paste0("stability_", check_metric(metric))]]
  d[c("factor_index", "label", "stability", "n_genes")]
}

# PCA's per-rank reconstruction error and cumulative variance explained, from fit_diag
# of its single max-rank fit.
pca_scree <- function(db, dataset_id) {
  f <- db_query(
    db,
    "SELECT fit_id FROM fits
     WHERE dataset_id = ? AND method = 'pca' AND status = 'ok' ORDER BY rank DESC LIMIT 1",
    params = list(dataset_id)
  )
  if (nrow(f) == 0) {
    return(NULL)
  }
  d <- db_query(
    db, "SELECT name, idx, value FROM fit_diag WHERE fit_id = ? ORDER BY idx",
    params = list(f$fit_id)
  )
  mse <- d[d$name == "mse_by_rank", ]
  sdev <- d[d$name == "sdev", ]
  if (nrow(mse) == 0 || nrow(sdev) == 0) {
    return(NULL)
  }
  k <- mse$idx
  data.frame(rank = k, mse = mse$value, pev = (cumsum(sdev$value^2) / sum(sdev$value^2))[k])
}

# sPCA's Index-of-Sparseness inputs for every fit: rank, para, PEV of the sparse and
# ordinary components, proportion sparse, IS, and whether chosen.
spca_is_table <- function(db, dataset_id) {
  d <- db_query(
    db,
    "SELECT f.fit_id, f.rank, f.para, m.name, m.value FROM v_fits f
     JOIN fit_metrics m ON m.fit_id = f.fit_id
     WHERE f.dataset_id = ? AND f.method = 'spca' AND f.status = 'ok'
       AND m.name IN ('pev_sparse', 'pev_pca', 'prop_sparse', 'index_of_sparseness',
                      'is_selected')",
    params = list(dataset_id)
  )
  if (nrow(d) == 0) {
    return(NULL)
  }
  w <- stats::reshape(d, idvar = c("fit_id", "rank", "para"), timevar = "name", direction = "wide")
  names(w) <- sub("^value\\.", "", names(w))
  w
}

# ICASSO cluster quality (Iq) per rank, one row per cluster.
icasso_iq <- function(db, dataset_id) {
  db_query(
    db,
    "SELECT rank, cluster_id, iq, n_members FROM icasso_clusters
     WHERE dataset_id = ? ORDER BY rank, cluster_id",
    params = list(dataset_id)
  )
}

# WGCNA: ARI between every pair of powers (symmetric matrix).
wgcna_ari_matrix <- function(db, dataset_id) {
  d <- db_query(
    db,
    "SELECT ra.power AS power_a, rb.power AS power_b, p.ari
     FROM fit_pairs p JOIN v_fits ra ON ra.fit_id = p.fit_a JOIN v_fits rb ON rb.fit_id = p.fit_b
     WHERE ra.dataset_id = ? AND ra.method = 'wgcna'",
    params = list(dataset_id)
  )
  if (nrow(d) == 0) {
    return(NULL)
  }
  pw <- as.character(sort(unique(c(d$power_a, d$power_b))))
  m <- matrix(NA_real_, length(pw), length(pw), dimnames = list(pw, pw))
  m[cbind(match(d$power_a, pw), match(d$power_b, pw))] <- d$ari
  m[cbind(match(d$power_b, pw), match(d$power_a, pw))] <- d$ari
  diag(m) <- 1
  m
}

# WGCNA per-power summary: scale-free fit, modules, unassigned genes.
wgcna_power_summary <- function(db, dataset_id) {
  db_query(
    db,
    "SELECT f.power, f.n_factors AS n_modules, s.sft_r_sq, u.value AS n_unassigned
     FROM v_fits f
     LEFT JOIN wgcna_sft s ON s.dataset_id = f.dataset_id AND s.power = f.power
     LEFT JOIN fit_metrics u ON u.fit_id = f.fit_id AND u.name = 'n_unassigned'
     WHERE f.dataset_id = ? AND f.method = 'wgcna' AND f.status = 'ok' ORDER BY f.power",
    params = list(dataset_id)
  )
}

## ---- factors of one fit ------------------------------------------------------------------

# Factors of a fit; `n` keeps the first n (PCA's nested fit at a smaller rank).
fit_factors <- function(db, fit_id, n = NULL) {
  d <- db_query(
    db,
    "SELECT factor_id, factor_index, label, n_genes, stability_cosine, stability_pearson,
            stability_spearman, kurtosis, excess_kurtosis
     FROM factors WHERE fit_id = ? ORDER BY factor_index",
    params = list(fit_id)
  )
  if (!is.null(n) && !is.na(n)) {
    d <- d[d$factor_index <= n, , drop = FALSE]
  }
  d
}

# One factor's match in every other fit of its method (both orientations of
# `v_factor_matches`): other fit's rank/seed, whether it is the same rank, and the signed
# value of `metric`.
factor_matches_everywhere <- function(db, factor_id, metric = "cosine") {
  db_query(
    db,
    "SELECT m.other_rank, f.power AS other_power, m.other_seed, m.other_fit_id,
            m.other_factor_index, m.same_rank, m.value, m.runner_up, m.margin
     FROM v_factor_matches m JOIN v_fits f ON f.fit_id = m.other_fit_id
     WHERE m.factor_id = ? AND m.metric = ?",
    params = list(factor_id, check_metric(metric, MATCH_METRICS))
  )
}

## ---- enrichment ---------------------------------------------------------------------------------

QUERY_LABELS <- c(
  fgsea = "fgsea (ranked loadings)", gsea = "GSEA", ora = "over-representation",
  cogaps_fora = "CoGAPS marker over-representation"
)

# query_type x direction combinations with results for any factor of a fit.
enrichment_queries_for_fit <- function(db, fit_id, n = NULL) {
  d <- db_query(
    db,
    "SELECT DISTINCT q.query_type, q.direction FROM enrichment_queries q
     JOIN factors x ON x.factor_id = q.factor_id WHERE x.fit_id = ? AND x.factor_index <= ?",
    params = list(fit_id, n %||% 1e9)
  )
  d[order(d$query_type, d$direction), , drop = FALSE]
}

# Stored enrichment terms for one factor (only BH-adjusted p < 0.05 are kept).
factor_enrichment <- function(db, factor_id, query_type, direction) {
  db_query(
    db,
    "SELECT r.source, r.term_id, r.term_name, r.p_value, r.nes, r.intersection_size, r.term_size
     FROM enrichment_results r JOIN enrichment_queries q ON q.query_id = r.query_id
     WHERE q.factor_id = ? AND q.query_type = ? AND q.direction = ? ORDER BY r.p_value",
    params = list(factor_id, query_type, direction)
  )
}

# Enrichment for every factor of a fit in one query (long, with factor_index/label).
fit_enrichment <- function(db, fit_id, query_type, direction, n = NULL) {
  db_query(
    db,
    "SELECT x.factor_index, x.label, r.source, r.term_name, r.p_value, r.nes
     FROM factors x JOIN enrichment_queries q ON q.factor_id = x.factor_id
     JOIN enrichment_results r ON r.query_id = q.query_id
     WHERE x.fit_id = ? AND x.factor_index <= ? AND q.query_type = ? AND q.direction = ?",
    params = list(fit_id, n %||% 1e9, query_type, direction)
  )
}

## ---- WGCNA -------------------------------------------------------------------------

wgcna_fits <- function(db, dataset_id) {
  db_query(
    db,
    "SELECT fit_id, power, n_factors FROM v_fits
     WHERE dataset_id = ? AND method = 'wgcna' AND status = 'ok' ORDER BY power",
    params = list(dataset_id)
  )
}

# Gene -> module assignment with kME for one fit.
wgcna_gene_modules <- function(db, fit_id) {
  db_query(
    db,
    "SELECT gene, module, kme_own, next_module, kme_next, margin, p_analytic, p_perm
     FROM gene_modules WHERE fit_id = ?",
    params = list(fit_id)
  )
}

# kME summary of members, non-members and the permutation null per module.
wgcna_kme_summary <- function(db, fit_id) {
  db_query(
    db,
    "SELECT module, grp, n, mean, median, p95 FROM module_kme_summary
     WHERE fit_id = ? AND module != 0",
    params = list(fit_id)
  )
}

wgcna_kme_histograms <- function(db, fit_id, module) {
  db_query(
    db,
    "SELECT grp, bin_lo, bin_hi, count FROM module_kme_histograms
     WHERE fit_id = ? AND module = ?",
    params = list(fit_id, module)
  )
}

# Fields with gene-significance tests for a dataset.
gene_significance_fields <- function(db, dataset_id) {
  db_query(
    db,
    "SELECT DISTINCT field, test FROM gene_significance WHERE dataset_id = ? ORDER BY field",
    params = list(dataset_id)
  )
}

gene_significance_for <- function(db, dataset_id, field) {
  db_query(
    db,
    "SELECT gene, statistic, p_value FROM gene_significance WHERE dataset_id = ? AND field = ?",
    params = list(dataset_id, field)
  )
}

# Jaccard overlap of the modules of two WGCNA fits (rows: fit a, cols: fit b).
wgcna_module_overlap <- function(db, fit_a, fit_b) {
  ma <- wgcna_gene_modules(db, fit_a)
  mb <- wgcna_gene_modules(db, fit_b)
  sa <- split(ma$gene[ma$module != 0], ma$module[ma$module != 0])
  sb <- split(mb$gene[mb$module != 0], mb$module[mb$module != 0])
  if (length(sa) == 0 || length(sb) == 0) {
    return(NULL)
  }
  names(sa) <- paste0("ME", names(sa))
  names(sb) <- paste0("ME", names(sb))
  jaccard_matrix(sa, sb)
}

jaccard <- function(a, b) {
  u <- union(a, b)
  if (length(u) == 0) {
    return(NA_real_)
  }
  length(intersect(a, b)) / length(u)
}

jaccard_matrix <- function(sets_a, sets_b) {
  m <- matrix(
    NA_real_, length(sets_a), length(sets_b),
    dimnames = list(names(sets_a), names(sets_b))
  )
  for (i in seq_along(sets_a)) {
    for (j in seq_along(sets_b)) {
      m[i, j] <- jaccard(sets_a[[i]], sets_b[[j]])
    }
  }
  m
}
