# Derive steps that reduce or repack artifacts (see R/derive.R). Each takes the
# derive context and returns the number of rows it wrote.

#' Gene numbers of a dataset
#'
#' `genes` holds the id and symbol of every row of the dataset's feature
#' metadata, numbered in file order. Reduced tables refer to genes by
#' `gene_idx`; an id that is not here is an error.
#'
#' @noRd
derive_genes <- function(ctx) {
  con <- ctx$con
  d <- ctx$dataset
  fm <- read_dataset_artifact(ctx, "feature_metadata")
  id_col <- d$feature_id_col
  if (is.null(fm) || is.na(id_col) || !id_col %in% names(fm)) {
    cli::cli_abort(
      c(
        "Cannot number the genes of {.val {ctx$dataset_id}}.",
        "x" = paste0(
          "The dataset needs a {.field feature_metadata} artifact with the column ",
          "{.field {id_col}} (its {.field feature_id_col})."
        ),
        "i" = "Reduced tables refer to genes by number, so the ids must be known."
      ),
      class = "sepsisfactorsdb_invalid_results"
    )
  }
  ids <- as_id(fm[[id_col]])
  if (anyNA(ids) || anyDuplicated(ids)) {
    cli::cli_abort(
      "The feature ids of {.val {ctx$dataset_id}} ({.field {id_col}}) are missing or not unique.",
      class = "sepsisfactorsdb_invalid_results"
    )
  }
  sym <- if (!is.na(d$symbol_col) && d$symbol_col %in% names(fm)) {
    as.character(fm[[d$symbol_col]])
  } else {
    rep(NA_character_, length(ids))
  }
  sym[!is.na(sym) & !nzchar(sym)] <- NA_character_
  DBI::dbExecute(con, "DELETE FROM genes WHERE dataset_id = ?", params = list(ctx$dataset_id))
  idx <- seq_along(ids)
  DBI::dbAppendTable(
    con,
    "genes",
    data.frame(dataset_id = ctx$dataset_id, gene_idx = idx, gene_id = ids, symbol = sym)
  )
  length(ids)
}

#' Gene numbers of ids, failing on ids that are not genes of the dataset
#'
#' @param ctx A derive context whose `genes` has been set from `load_gene_lookup()`.
#' @param ids Feature ids.
#' @param what What the ids belong to, for the message.
#' @noRd
gene_idx_of <- function(ctx, ids, what) {
  out <- unname(ctx$genes$lookup[as.character(ids)])
  bad <- unique(ids[is.na(out)])
  if (length(bad)) {
    cli::cli_abort(
      c(
        paste0(
          "{length(bad)} gene id{?s} in {what} of {.val {ctx$dataset_id}} {?is/are} ",
          "not in the dataset's feature metadata."
        ),
        "i" = "First: {.val {utils::head(bad, 5)}}"
      ),
      class = "sepsisfactorsdb_unmapped_key"
    )
  }
  out
}

#' Sample metadata, long
#'
#' @noRd
derive_sample_metadata <- function(ctx) {
  con <- ctx$con
  DBI::dbExecute(
    con,
    "DELETE FROM sample_metadata WHERE dataset_id = ?",
    params = list(ctx$dataset_id)
  )
  m <- read_dataset_artifact(ctx, "sample_metadata")
  id_col <- ctx$dataset$sample_id_col
  if (is.null(m) || is.na(id_col) || !id_col %in% names(m)) {
    return(0L)
  }
  m <- as.data.frame(m)
  ids <- as.character(m[[id_col]])
  fields <- setdiff(names(m), id_col)
  rows <- lapply(fields, function(f) {
    v <- m[[f]]
    data.frame(
      dataset_id = ctx$dataset_id,
      sample_id = ids,
      field = f,
      type = class(v)[1],
      value = if (is.logical(v) || is.numeric(v) || is.factor(v) || is.character(v)) {
        as.character(
          v
        )
      } else {
        format(v)
      },
      stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, rows)
  if (is.null(rows) || nrow(rows) == 0L) {
    return(0L)
  }
  rows$value[is.na(rows$value) | rows$value == "NA" & rows$type %in% c(
    "numeric",
    "integer",
    "logical"
  )] <- NA_character_
  DBI::dbAppendTable(con, "sample_metadata", rows)
  nrow(rows)
}

#' Top genes of each factor on each side of zero
#'
#' @noRd
derive_factor_top_genes <- function(ctx, top = TOP_GENES_PER_SIDE) {
  con <- ctx$con
  DBI::dbExecute(
    con,
    "DELETE FROM factor_top_genes WHERE fit_id IN (SELECT fit_id FROM fits WHERE dataset_id = ?)",
    params = list(ctx$dataset_id)
  )
  n <- 0L
  for (fit_id in ctx$fit_files$fit_id[ctx$fit_files$kind == "loadings"]) {
    L <- read_fit_artifact(ctx, fit_id, "loadings")
    if (is.null(rownames(L))) {
      cli::cli_abort("The loadings of fit {fit_id} in {.val {ctx$dataset_id}} have no gene names.")
    }
    gi <- gene_idx_of(ctx, rownames(L), "the loadings")
    rows <- lapply(seq_len(ncol(L)), function(j) top_gene_rows(L[, j], gi, fit_id, j, top))
    rows <- do.call(rbind, rows)
    if (!is.null(rows) && nrow(rows)) {
      DBI::dbAppendTable(con, "factor_top_genes", rows)
      n <- n + nrow(rows)
    }
  }
  n
}

#' The top positive and negative genes of one loadings column
#'
#' @noRd
top_gene_rows <- function(v, gene_idx, fit_id, factor_index, top) {
  ord <- order(v, decreasing = TRUE)
  pos <- utils::head(ord[v[ord] > 0 & !is.na(v[ord])], top)
  rev_ord <- rev(ord)
  neg <- utils::head(rev_ord[v[rev_ord] < 0 & !is.na(v[rev_ord])], top)
  side <- function(i, name) {
    if (!length(i)) {
      return(NULL)
    }
    data.frame(
      fit_id = fit_id,
      factor_index = factor_index,
      side = name,
      rank = seq_along(i),
      gene_idx = gene_idx[i],
      loading = v[i]
    )
  }
  rbind(side(pos, "pos"), side(neg, "neg"))
}

#' Sample scores of each fit as a float32 blob
#'
#' @noRd
derive_fit_scores <- function(ctx) {
  con <- ctx$con
  DBI::dbExecute(
    con,
    "DELETE FROM fit_scores WHERE fit_id IN (SELECT fit_id FROM fits WHERE dataset_id = ?)",
    params = list(ctx$dataset_id)
  )
  ids <- ctx$fit_files$fit_id[ctx$fit_files$kind == "scores"]
  n <- 0L
  for (chunk in split(ids, ceiling(seq_along(ids) / 100))) {
    mats <- lapply(chunk, function(i) {
      S <- read_fit_artifact(ctx, i, "scores")
      if (is.null(rownames(S))) {
        cli::cli_abort("The scores of fit {i} in {.val {ctx$dataset_id}} have no sample names.")
      }
      S
    })
    n <- n + insert_columns(
      con,
      "fit_scores",
      list(
        fit_id = chunk,
        row_ids = vapply(mats, function(S) json_ids(rownames(S)), ""),
        n_factors = vapply(mats, ncol, 1L),
        data = lapply(mats, encode_f32)
      )
    )
  }
  n
}

#' PCA's per-rank reconstruction error and standard deviations
#'
#' @noRd
derive_fit_diag <- function(ctx) {
  con <- ctx$con
  DBI::dbExecute(
    con,
    "DELETE FROM fit_diag WHERE fit_id IN (SELECT fit_id FROM fits WHERE dataset_id = ?)",
    params = list(ctx$dataset_id)
  )
  pca <- ctx$fits$fit_id[ctx$fits$method == "pca" & ctx$fits$status == "ok"]
  n <- 0L
  for (fit_id in intersect(pca, ctx$fit_files$fit_id[ctx$fit_files$kind == "diag"])) {
    d <- read_fit_artifact(ctx, fit_id, "diag")
    rows <- do.call(
      rbind,
      lapply(c("mse_by_rank", "sdev"), function(name) {
        v <- d[[name]]
        if (is.null(v)) {
          NULL
        } else {
          data.frame(
            fit_id = fit_id,
            name = name,
            idx = seq_along(v),
            value = as.numeric(v)
          )
        }
      })
    )
    if (!is.null(rows)) {
      DBI::dbAppendTable(con, "fit_diag", rows)
      n <- n + nrow(rows)
    }
  }
  n
}

#' WGCNA gene dendrograms as blobs
#'
#' @noRd
derive_wgcna_dendrograms <- function(ctx) {
  con <- ctx$con
  DBI::dbExecute(
    con,
    "DELETE FROM wgcna_dendrograms WHERE fit_id IN (SELECT fit_id FROM fits WHERE dataset_id = ?)",
    params = list(ctx$dataset_id)
  )
  n <- 0L
  for (fit_id in ctx$fit_files$fit_id[ctx$fit_files$kind == "dendrogram"]) {
    d <- read_fit_artifact(ctx, fit_id, "dendrogram")
    gi <- gene_idx_of(ctx, d$labels, "a gene dendrogram")
    n <- n + insert_columns(
      con,
      "wgcna_dendrograms",
      list(
        fit_id = fit_id,
        n = length(d$order),
        merge = list(encode_i32(d$merge)),
        height = list(encode_f32(d$height)),
        ord = list(encode_i32(d$order)),
        gene_idx = list(encode_i32(gi))
      )
    )
  }
  n
}

#' Reduced pattern-driver results
#'
#' PV mode keeps the genes that are shared-significant or have Welch adjusted
#' p below `DRIVER_PADJ`; CI mode keeps the `DRIVER_CI_TOP` genes with the
#' largest interval midpoint and the shared genes. `weight` is the normalised
#' weight of every kept gene.
#'
#' @noRd
derive_driver_genes <- function(ctx) {
  con <- ctx$con
  d <- DBI::dbGetQuery(
    con,
    "SELECT d.driver_id, d.path, d.mode, f.fit_id FROM pattern_drivers d
     JOIN factors x ON x.factor_id = d.factor_id JOIN fits f ON f.fit_id = x.fit_id
     WHERE f.dataset_id = ?",
    params = list(ctx$dataset_id)
  )
  d <- d[d$fit_id %in% ctx$fits$fit_id, , drop = FALSE]
  if (nrow(d)) {
    DBI::dbExecute(
      con,
      sprintf(
        "DELETE FROM driver_genes WHERE driver_id IN (%s)",
        paste(d$driver_id, collapse = ",")
      )
    )
  }
  n <- 0L
  for (i in seq_len(nrow(d))) {
    res <- readRDS(resolve_artifact(d$path[i], ctx$artifact_base))
    rows <- reduce_driver(res, d$mode[i])
    if (!nrow(rows)) {
      next
    }
    rows$gene_idx <- gene_idx_of(ctx, rows$gene, "a pattern driver result")
    rows$driver_id <- d$driver_id[i]
    DBI::dbAppendTable(
      con,
      "driver_genes",
      rows[, c("driver_id", "gene_idx", "mean_diff", "padj", "weight", "shared", "low", "high")]
    )
    n <- n + nrow(rows)
  }
  n
}

#' The rows of `driver_genes` for one projectionDriveR result
#'
#' @param res The driver file's contents.
#' @param mode `"PV"` or `"CI"`.
#' @return A data frame: `gene` (feature id), `mean_diff`, `padj`, `weight`,
#'   `shared`, `low`, `high`.
#' @noRd
reduce_driver <- function(res, mode) {
  shared <- as.character(
    res$sig_genes$significant_shared_genes %||% res$sig_genes$PV_significant_shared_genes
  )
  w <- res$normalized_weights
  weight_of <- function(g) as.numeric(w[g])
  if (mode == "PV") {
    ms <- res$weighted_mean_stats
    keep <- ms$gene %in% shared | (!is.na(ms$welch_padj) & ms$welch_padj < DRIVER_PADJ)
    ms <- ms[keep, , drop = FALSE]
    out <- data.frame(
      gene = as.character(ms$gene),
      mean_diff = ms$mean_diff,
      padj = ms$welch_padj,
      weight = weight_of(ms$gene),
      shared = as.integer(ms$gene %in% shared),
      low = NA_real_,
      high = NA_real_
    )
    absent <- setdiff(shared, out$gene)
    if (length(absent)) {
      out <- rbind(
        out,
        data.frame(
          gene = absent,
          mean_diff = NA_real_,
          padj = NA_real_,
          weight = weight_of(absent),
          shared = 1L,
          low = NA_real_,
          high = NA_real_
        )
      )
    }
    return(out)
  }
  ci <- res$weighted_mean_ci
  top <- utils::head(order(-abs(ci$low + ci$high)), DRIVER_CI_TOP)
  keep <- sort(unique(c(top, which(ci$gene %in% shared))))
  ci <- ci[keep, , drop = FALSE]
  data.frame(
    gene = as.character(ci$gene),
    mean_diff = NA_real_,
    padj = NA_real_,
    weight = weight_of(ci$gene),
    shared = as.integer(ci$gene %in% shared),
    low = ci$low,
    high = ci$high
  )
}

#' Per-gene variance shares used by the variance page
#'
#' Reads `gene_summary` of the dataset's `variance_partition` artifact (only
#' datasets with a variance partition have one) and keeps `time_all` and
#' `factor_gene`. The partition names genes by id or, for some datasets, by
#' symbol; each is matched against the dataset's genes, ids first.
#'
#' @noRd
derive_gene_variance <- function(ctx) {
  con <- ctx$con
  DBI::dbExecute(
    con,
    "DELETE FROM gene_variance WHERE dataset_id = ?",
    params = list(ctx$dataset_id)
  )
  vp <- read_dataset_artifact(ctx, "variance_partition")
  gs <- vp$gene_summary
  if (is.null(gs)) {
    return(0L)
  }
  g <- as.character(gs$gene)
  idx <- unname(ctx$genes$lookup[g])
  by_symbol <- is.na(idx)
  idx[by_symbol] <- unname(ctx$genes$by_symbol[g[by_symbol]])
  if (anyNA(idx)) {
    bad <- unique(g[is.na(idx)]) # nolint: object_usage_linter. Used by the cli glue below.
    cli::cli_abort(
      c(
        paste0(
          "{length(bad)} gene{?s} of the variance partition of {.val {ctx$dataset_id}} ",
          "{?is/are} not in its feature metadata (by id or symbol)."
        ),
        "i" = "First: {.val {utils::head(bad, 5)}}"
      ),
      class = "sepsisfactorsdb_unmapped_key"
    )
  }
  rows <- data.frame(
    dataset_id = ctx$dataset_id,
    gene_idx = idx,
    time_all = gs$time_all,
    factor_gene = as.integer(gs$factor_gene)
  )
  rows <- rows[!duplicated(rows$gene_idx), , drop = FALSE]
  DBI::dbAppendTable(con, "gene_variance", rows)
  nrow(rows)
}
