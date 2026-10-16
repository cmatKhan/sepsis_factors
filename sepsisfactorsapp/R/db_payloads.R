# Loadings, scores, projections, pattern drivers, sample metadata and dendrograms.
# Where a full artifact may sit beside the database (loadings, scores, projections and
# pattern drivers), the reader uses it when its file exists and otherwise the reduced
# copy in the database; the result carries its source (see with_source()).

first_cols <- function(m, n) {
  if (is.null(n) || is.na(n)) m else m[, seq_len(min(n, ncol(m))), drop = FALSE]
}

# The path of a fit's full artifact of one kind (NA when the database has none).
fit_pointer <- function(db, fit_id, kind) {
  p <- db_query(
    db, "SELECT path FROM fit_artifacts WHERE fit_id = ? AND kind = ?",
    params = list(fit_id, kind)
  )$path
  if (length(p) == 1) p else NA_character_
}

## ---- loadings ----------------------------------------------------------------------------

# The stored top genes of every factor of a fit (50 per side), with the gene ids.
top_gene_rows <- function(db, fit_id) {
  db_query(
    db,
    "SELECT t.factor_index, t.side, t.rank, g.gene_id, t.loading
     FROM factor_top_genes t
     JOIN fits f ON f.fit_id = t.fit_id
     JOIN genes g ON g.dataset_id = f.dataset_id AND g.gene_idx = t.gene_idx
     WHERE t.fit_id = ?",
    params = list(fit_id)
  )
}

# A genes x factors matrix from the stored top genes. A gene that is not among a factor's
# stored genes gets 0 on it, so this is a sparse view of the loadings; only the genes
# stored for some factor are rows.
sparse_loadings <- function(rows, n = NULL) {
  if (!is.null(n) && !is.na(n)) {
    rows <- rows[rows$factor_index <= n, , drop = FALSE]
  }
  if (nrow(rows) == 0) {
    return(NULL)
  }
  genes <- unique(rows$gene_id)
  L <- matrix(0, length(genes), max(rows$factor_index), dimnames = list(genes, NULL))
  L[cbind(match(rows$gene_id, genes), rows$factor_index)] <- rows$loading
  L
}

# A fit's loadings (genes x factors); NULL for a method without loadings. `n` keeps the
# first n factors, which is how PCA's single max-rank fit is viewed at a smaller rank.
# From the database copy, the matrix holds only the stored top genes (see
# sparse_loadings()) and its source is "database".
load_loadings <- function(db, fit_id, n = NULL) {
  full <- read_artifact(db, fit_pointer(db, fit_id, "loadings"))
  if (!is.null(full)) {
    return(with_source(first_cols(full, n), "artifact"))
  }
  L <- sparse_loadings(top_gene_rows(db, fit_id), n)
  if (is.null(L)) NULL else with_source(L, "database")
}

# Top genes of one loadings column on each side. Returns data.frame(gene, loading, side).
# Non-negative methods (all loadings >= 0) only have "positive".
top_loading_genes <- function(L, j, n = 20) {
  v <- L[, j]
  pos <- utils::head(order(v, decreasing = TRUE), n)
  d <- data.frame(
    gene = rownames(L)[pos], loading = v[pos], side = "positive", stringsAsFactors = FALSE
  )
  if (any(v < 0)) {
    neg <- utils::head(order(v), n)
    d <- rbind(d, data.frame(
      gene = rownames(L)[neg], loading = v[neg], side = "negative", stringsAsFactors = FALSE
    ))
  }
  d
}

## ---- scores -------------------------------------------------------------------------------

# Name the columns of a decoded scores blob by the factor labels, in factor order; columns
# beyond the labels (WGCNA eigengenes outside the numbered modules) get "extra<k>".
name_score_columns <- function(S, labels) {
  k <- ncol(S)
  extra <- if (k > length(labels)) paste0("extra", seq_len(k - length(labels))) else NULL
  colnames(S) <- c(labels, extra)[seq_len(k)]
  S
}

decode_scores <- function(row, labels) {
  ids <- decode_ids(row$row_ids)
  S <- matrix(decode_f32(row$data[[1]]), nrow = length(ids), ncol = row$n_factors)
  rownames(S) <- ids
  name_score_columns(S, labels)
}

# A fit's sample scores (samples x factors; module eigengenes for WGCNA). From the
# database copy the values are complete but single precision.
load_scores <- function(db, fit_id, n = NULL) {
  full <- read_artifact(db, fit_pointer(db, fit_id, "scores"))
  if (!is.null(full)) {
    return(with_source(first_cols(full, n), "artifact"))
  }
  row <- db_query(
    db, "SELECT row_ids, n_factors, data FROM fit_scores WHERE fit_id = ?",
    params = list(fit_id)
  )
  if (nrow(row) == 0) {
    return(NULL)
  }
  labels <- fit_factors(db, fit_id)$label
  with_source(first_cols(decode_scores(row, labels), n), "database")
}

# Sample scores of the focus fit with columns named by factor label (WGCNA's eigengene
# columns are in a different order from factor_index).
focus_scores <- function(db, focus) {
  S <- load_scores(db, focus$fit_id)
  if (is.null(S)) {
    return(NULL)
  }
  fac <- fit_factors(db, focus$fit_id, focus$n_factors)
  if (!focus$has_loadings) {
    return(S[, intersect(fac$label, colnames(S)), drop = FALSE])
  }
  S <- S[, seq_len(min(nrow(fac), ncol(S))), drop = FALSE]
  colnames(S) <- fac$label[seq_len(ncol(S))]
  S
}

## ---- projections ----------------------------------------------------------------------------

# A projection result from its blob: the weights (components x samples), their p values
# and the r squared per sample.
decode_projection <- function(row) {
  comps <- decode_ids(row$row_ids)
  samples <- decode_ids(row$col_ids)
  v <- decode_f32(row$data[[1]])
  n <- length(comps) * length(samples)
  mat <- function(x) matrix(x, length(comps), length(samples), dimnames = list(comps, samples))
  list(
    projection = mat(v[seq_len(n)]),
    pval = mat(v[n + seq_len(n)]),
    r_squared = stats::setNames(v[2 * n + seq_along(samples)], samples)
  )
}

# The projected weights (components x samples) of one projection, keeping the first `n`
# components. Complete in both sources.
load_projection <- function(db, projection_id, path, n = NULL) {
  full <- read_artifact(db, path)
  if (!is.null(full)) {
    P <- full$projection
  } else {
    row <- db_query(
      db, "SELECT row_ids, col_ids, data FROM projection_values WHERE projection_id = ?",
      params = list(projection_id)
    )
    P <- if (nrow(row) == 1) decode_projection(row)$projection else NULL
  }
  if (!is.null(P) && !is.null(n) && !is.na(n)) {
    P <- P[seq_len(min(n, nrow(P))), , drop = FALSE]
  }
  P
}

# Within-family projections of one source fit (its basis projected onto the sibling
# per-timepoint datasets).
within_projections <- function(db, fit_id) {
  db_query(
    db,
    paste(
      "SELECT projection_id, target_dataset_id, path FROM projections",
      "WHERE source_fit_id = ? AND projection_type = 'within_dataset' AND include_intercept = 0",
      "ORDER BY target_dataset_id"
    ),
    params = list(fit_id)
  )
}

projection_targets <- function(db, dataset_id) {
  db_query(
    db,
    paste(
      "SELECT DISTINCT target_dataset_id FROM v_projections",
      "WHERE source_dataset_id = ? AND", PROJECTION_VARIANT_SQL, "ORDER BY target_dataset_id"
    ),
    params = list(dataset_id)
  )$target_dataset_id
}

# Every projection of a dataset's fits onto one target, with the source fit's
# method/rank/seed and whether it is the representative fit.
projections_for_pair <- function(db, dataset_id, target) {
  db_query(
    db,
    paste(
      "SELECT projection_id, source_fit_id, method, rank, seed, representative,",
      "projection_type, relative_r2, path FROM v_projections",
      "WHERE source_dataset_id = ? AND target_dataset_id = ? AND", PROJECTION_VARIANT_SQL
    ),
    params = list(dataset_id, target)
  )
}

# Per-subject weight of one projected component at every timepoint dataset (needs each
# target's subject id column). `component` is the row name of the projection matrix.
# NULL when nothing is usable.
trajectory_data <- function(db, fit_id, component) {
  tg <- within_projections(db, fit_id)
  rows <- lapply(seq_len(nrow(tg)), function(i) {
    ds <- tg$target_dataset_id[i]
    P <- load_projection(db, tg$projection_id[i], tg$path[i])
    sc <- dataset_subject_col(db, ds)
    m <- dataset_sample_metadata(db, ds)
    if (is.null(P) || !component %in% rownames(P)) {
      return(NULL)
    }
    if (is.null(m) || is.na(sc) || !sc %in% names(m)) {
      return(NULL)
    }
    w <- P[component, ]
    subj <- m[[sc]][match(names(w), m$sample_id)]
    ok <- !is.na(subj)
    if (!any(ok)) {
      return(NULL)
    }
    data.frame(
      dataset_id = ds, subject_id = as.character(subj[ok]), weight = unname(w[ok]),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) NULL else do.call(rbind, rows)
}

## ---- pattern drivers ----------------------------------------------------------------------------

# Stored projectionDriveR results for the factors of one fit.
fit_drivers <- function(db, fit_id, n = NULL) {
  d <- db_query(
    db,
    "SELECT d.driver_id, x.factor_index, x.label, d.grouping_col, d.group1_level, d.group2_level,
            d.mode, d.path
     FROM pattern_drivers d JOIN factors x ON x.factor_id = d.factor_id
     WHERE x.fit_id = ?
     ORDER BY x.factor_index, d.grouping_col, d.group1_level, d.group2_level, d.mode",
    params = list(fit_id)
  )
  if (!is.null(n) && !is.na(n)) {
    d <- d[d$factor_index <= n, , drop = FALSE]
  }
  d
}

# One projectionDriveR file as the table the database keeps: gene (feature id),
# mean_diff, padj, weight, shared, low, high. From the file, every gene is kept.
normalize_driver <- function(res, mode) {
  shared <- as.character(
    res$sig_genes$significant_shared_genes %||% res$sig_genes$PV_significant_shared_genes
  )
  w <- res$normalized_weights
  if (mode == "PV") {
    ms <- res$weighted_mean_stats
    gene <- as.character(ms$gene)
    return(data.frame(
      gene = gene, mean_diff = ms$mean_diff, padj = ms$welch_padj, weight = as.numeric(w[gene]),
      shared = gene %in% shared, low = NA_real_, high = NA_real_, stringsAsFactors = FALSE
    ))
  }
  ci <- res$weighted_mean_ci
  gene <- as.character(ci$gene)
  data.frame(
    gene = gene, mean_diff = NA_real_, padj = NA_real_, weight = as.numeric(w[gene]),
    shared = gene %in% shared, low = ci$low, high = ci$high, stringsAsFactors = FALSE
  )
}

# The result of one stored driver (a row of fit_drivers()) in the form of
# normalize_driver(), from the full file when it exists, else from `driver_genes`
# (PV: significant genes only; CI: the top 25 and the shared genes). NULL when neither.
load_driver <- function(db, driver) {
  res <- read_artifact(db, driver$path)
  if (!is.null(res)) {
    return(with_source(normalize_driver(res, driver$mode), "artifact"))
  }
  d <- db_query(
    db,
    "SELECT g.gene_id AS gene, d.mean_diff, d.padj, d.weight, d.shared, d.low, d.high
     FROM driver_genes d
     JOIN pattern_drivers p ON p.driver_id = d.driver_id
     JOIN factors x ON x.factor_id = p.factor_id
     JOIN fits f ON f.fit_id = x.fit_id
     JOIN genes g ON g.dataset_id = f.dataset_id AND g.gene_idx = d.gene_idx
     WHERE d.driver_id = ?",
    params = list(driver$driver_id)
  )
  if (nrow(d) == 0) {
    return(NULL)
  }
  d$shared <- d$shared == 1
  with_source(d, "database")
}

## ---- sample metadata, dendrograms and gene variance ----------------------------

# Convert text values to the R class the database recorded.
cast_metadata <- function(x, type) {
  switch(type,
    numeric = as.numeric(x),
    integer = as.integer(x),
    logical = as.logical(x),
    factor = factor(x),
    x
  )
}

# The long `sample_metadata` rows (sample_id, field, type, value) as a data frame with
# one row per sample, a `sample_id` column and one typed column per field. NULL if empty.
wide_metadata <- function(rows) {
  if (nrow(rows) == 0) {
    return(NULL)
  }
  ids <- unique(rows$sample_id)
  out <- data.frame(sample_id = ids, stringsAsFactors = FALSE)
  for (field in unique(rows$field)) {
    r <- rows[rows$field == field, , drop = FALSE]
    out[[field]] <- cast_metadata(r$value[match(ids, r$sample_id)], r$type[1])
  }
  out
}

# Sample metadata with the id column named `sample_id`; NULL if the dataset has none.
dataset_sample_metadata <- function(db, dataset_id) {
  wide_metadata(db_query(
    db,
    "SELECT sample_id, field, type, value FROM sample_metadata WHERE dataset_id = ?",
    params = list(dataset_id)
  ))
}

# hclust object for the gene dendrogram of a WGCNA fit; leaves are labelled with gene ids.
wgcna_dendrogram <- function(db, fit_id, dataset_id) {
  row <- db_query(
    db, "SELECT merge, height, ord, gene_idx FROM wgcna_dendrograms WHERE fit_id = ?",
    params = list(fit_id)
  )
  if (nrow(row) == 0) {
    return(NULL)
  }
  genes <- dataset_genes(db, dataset_id)
  structure(
    list(
      merge = matrix(decode_i32(row$merge[[1]]), ncol = 2),
      height = decode_f32(row$height[[1]]),
      order = decode_i32(row$ord[[1]]),
      labels = genes$gene_id[match(decode_i32(row$gene_idx[[1]]), genes$gene_idx)],
      method = "average", dist.method = "unknown"
    ),
    class = "hclust"
  )
}

# Per-gene variance shares of one dataset (time_all, factor_gene).
gene_variance_for <- function(db, dataset_id) {
  d <- db_query(
    db, "SELECT time_all, factor_gene FROM gene_variance WHERE dataset_id = ?",
    params = list(dataset_id)
  )
  d$factor_gene <- d$factor_gene == 1
  d
}
