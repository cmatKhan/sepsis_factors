# The derive step (TEMPORARY, see the "TEMPORARY patches" section of the root
# TODO.md). After a dataset is loaded, the tables below are built from the
# full artifacts of the results output so that the app can run from the
# database alone. They are reductions and summaries of numbers `sepsisfactors`
# already computed, plus a few statistics the app used to compute live
# (association scan, cross-method agreement). This is the one place where
# sepsisfactorsdb computes; it moves into `sepsisfactors` with the results
# output v3.
#
#   genes, factor_top_genes, fit_scores, fit_diag, wgcna_dendrograms,
#   driver_genes, sample_metadata, gene_variance      (R/derive_payloads.R)
#   fit_summary, factor_association                   (R/derive_summaries.R)
#   method_agreement                                  from R/derive_agreement.R
#   projection_values                                 (cross-dataset, below)
#
# Every step deletes the rows it owns for the dataset and inserts them again,
# so running it twice leaves the same rows.

# Top genes kept per side of each factor.
TOP_GENES_PER_SIDE <- 50L
# Genes per factor at which cross-method agreement is precomputed.
AGREEMENT_TOP_N <- c(10L, 25L, 50L, 100L, 200L)
# Welch adjusted p below which a pattern-driver gene is kept (PV mode).
DRIVER_PADJ <- 0.05
# Genes kept by the largest interval midpoint (CI mode).
DRIVER_CI_TOP <- 25L

## ---- blobs -------------------------------------------------------------------------

#' Encode numbers as a compressed float32 blob
#'
#' Column-major values, 4-byte little-endian floats, `memCompress(type = "gzip")`.
#' `NA` is stored as NaN.
#'
#' @param x A numeric vector or matrix.
#' @return A raw vector.
#' @noRd
encode_f32 <- function(x) {
  memCompress(writeBin(as.numeric(x), raw(), size = 4L, endian = "little"), "gzip")
}

#' Encode integers as a compressed int32 blob
#'
#' @param x An integer vector or matrix.
#' @return A raw vector.
#' @noRd
encode_i32 <- function(x) {
  memCompress(writeBin(as.integer(x), raw(), size = 4L, endian = "little"), "gzip")
}

#' Decode a float32 blob
#'
#' @param blob A raw vector from `encode_f32()`.
#' @return A numeric vector (`NaN` becomes `NA`).
#' @noRd
decode_f32 <- function(blob) {
  raw <- memDecompress(blob, "gzip", asChar = FALSE)
  out <- readBin(raw, "numeric", n = length(raw) %/% 4L, size = 4L, endian = "little")
  out[is.nan(out)] <- NA_real_
  out
}

#' Decode an int32 blob
#'
#' @param blob A raw vector from `encode_i32()`.
#' @return An integer vector.
#' @noRd
decode_i32 <- function(blob) {
  raw <- memDecompress(blob, "gzip", asChar = FALSE)
  readBin(raw, "integer", n = length(raw) %/% 4L, size = 4L, endian = "little")
}

#' Insert rows with blob columns
#'
#' @param con DB connection.
#' @param table Table name.
#' @param cols Named list of columns (length 1 is recycled); a blob column is a
#'   list of raw vectors.
#' @return Number of rows inserted, invisibly.
#' @noRd
insert_columns <- function(con, table, cols) {
  n <- max(lengths(cols))
  if (any(lengths(cols) == 0L)) {
    return(invisible(0L))
  }
  cols <- lapply(cols, function(x) if (length(x) == 1L) rep(x, n) else x)
  sql <- sprintf(
    "INSERT INTO %s (%s) VALUES (%s)",
    table,
    paste(names(cols), collapse = ", "),
    paste(rep("?", length(cols)), collapse = ", ")
  )
  DBI::dbExecute(con, sql, params = unname(cols))
  invisible(n)
}

#' A character vector as a compact JSON array
#'
#' @noRd
json_ids <- function(x) {
  as.character(jsonlite::toJSON(as.character(x), auto_unbox = FALSE))
}

#' Feature ids as the text the loadings use
#'
#' Integer-valued numbers are written without exponents.
#'
#' @noRd
as_id <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x)) {
    return(ifelse(is.na(x), NA_character_, format(x, scientific = FALSE, trim = TRUE, digits = 15)))
  }
  as.character(x)
}

## ---- context -------------------------------------------------------------------------

#' What the derive step needs to know about one loaded dataset
#'
#' @param results_root The results directory.
#' @param con DB connection (after [load_dataset()]).
#' @param db_path Path to the database.
#' @param dataset_id Dataset id.
#' @param fit_keys Keys of the fits the fit-level steps are limited to (a chunk of
#'   a parallel build); `NULL` for all.
#' @param artifact_base Path of the database whose directory holds the placed
#'   artifacts.
#' @return A list used by the `derive_*` functions.
#' @noRd
derive_context <- function(
  results_root,
  con,
  db_path,
  dataset_id,
  fit_keys = NULL,
  artifact_base = db_path
) {
  ddir <- results_dataset_dir(results_root, dataset_id)
  files <- list_table_files(file.path(ddir, "tables"))
  tab <- function(name) if (name %in% names(files)) read_result_table(files[[name]]) else NULL
  fits <- DBI::dbGetQuery(
    con,
    "SELECT fit_id, fit_key, method, rank, bootstrap, representative, n_factors, status
     FROM fits WHERE dataset_id = ?",
    params = list(dataset_id)
  )
  fa <- tab("fit_artifacts")
  fit_files <- if (is.null(fa)) {
    data.frame(fit_id = integer(), kind = character(), file = character())
  } else {
    data.frame(
      fit_id = fits$fit_id[match(fa$fit_key, fits$fit_key)],
      kind = fa$kind,
      file = fa$path
    )
  }
  da <- tab("dataset_artifacts")
  dataset_files <- if (is.null(da)) character() else stats::setNames(da$path, da$kind)
  dataset <- DBI::dbGetQuery(
    con,
    "SELECT * FROM datasets WHERE dataset_id = ?",
    params = list(dataset_id)
  )
  all_fits <- fits
  if (!is.null(fit_keys)) {
    fits <- fits[fits$fit_key %in% fit_keys, , drop = FALSE]
  }
  fit_files <- fit_files[!is.na(fit_files$fit_id), , drop = FALSE]
  list(
    con = con,
    db_path = db_path,
    artifact_base = artifact_base,
    dataset_id = dataset_id,
    dataset = dataset,
    results_artifacts = file.path(ddir, "artifacts"),
    fits = fits,
    all_fits = all_fits,
    all_fit_files = fit_files,
    fit_files = fit_files[fit_files$fit_id %in% fits$fit_id, , drop = FALSE],
    dataset_files = dataset_files
  )
}

#' Read a fit's artifact
#'
#' The pointer kinds are read from the database's own tree (where
#' [load_dataset()] placed them, even if the mode moved them out of the
#' results); the others from the results output.
#'
#' @param ctx A `derive_context()`.
#' @param fit_id Fit id.
#' @param kind Artifact kind.
#' @return The object, or `NULL` when the fit has no such artifact.
#' @noRd
read_fit_artifact <- function(ctx, fit_id, kind) {
  files <- ctx$all_fit_files
  row <- files[files$fit_id == fit_id & files$kind == kind, , drop = FALSE]
  if (nrow(row) == 0L) {
    return(NULL)
  }
  readRDS(artifact_source(ctx, row$file[1], kind %in% POINTER_KINDS))
}

#' Read a dataset-level artifact (`sample_metadata`, `feature_metadata`, ...)
#'
#' @param ctx A `derive_context()`.
#' @param kind Artifact kind.
#' @return The object, or `NULL` when the dataset has none.
#' @noRd
read_dataset_artifact <- function(ctx, kind) {
  file <- ctx$dataset_files[kind]
  if (is.na(file)) {
    return(NULL)
  }
  readRDS(artifact_source(ctx, unname(file), FALSE))
}

#' Where an artifact file is
#'
#' @noRd
artifact_source <- function(ctx, file, pointer) {
  placed <- file.path(artifacts_dir(ctx$artifact_base, ctx$dataset_id), file)
  original <- file.path(ctx$results_artifacts, file)
  candidates <- if (pointer) c(placed, original) else c(original, placed)
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) {
    cli::cli_abort(
      paste0(
        "Artifact {.file {file}} of {.val {ctx$dataset_id}} is in neither the results ",
        "output nor the database tree."
      )
    )
  }
  hit[1]
}

## ---- running the step --------------------------------------------------------------------

#' Build the derived tables of one loaded dataset
#'
#' Run after [load_dataset()] (as [ingest_results()] does). Reads the full
#' artifacts of the results output and writes the tables the app needs to run
#' from the database alone: `genes`, `factor_top_genes`, `fit_scores`,
#' `fit_diag`, `wgcna_dendrograms`, `driver_genes`, `sample_metadata`,
#' `gene_variance`, `fit_summary`, `factor_association` and
#' `method_agreement`. One transaction; running it again replaces the rows.
#'
#' The tables split into dataset-level ones (`genes`, `sample_metadata`,
#' `gene_variance`) and fit-level ones (the rest). `level` and `fit_keys` let a
#' parallel build ([ingest_parallel()]) make the fit-level tables for one chunk
#' of fits at a time.
#'
#' This computes (reductions, an association scan, cross-method agreement), so
#' it is a temporary exception to "no statistics in sepsisfactorsdb"; the
#' `sepsisfactors` package takes it over when it writes the contract.
#'
#' @param results_root The results directory.
#' @param db_path Path to the SQLite file the dataset was loaded into.
#' @param dataset_id Dataset id.
#' @param level `"all"`, `"dataset"` (only the dataset-level tables) or `"fits"`
#'   (only the fit-level tables; needs `genes` already derived).
#' @param fit_keys With `level = "fits"`, the keys of the fits to derive; `NULL`
#'   for all. Cross-method agreement is made for the pairs whose first fit is in
#'   the chunk.
#' @inheritParams load_dataset
#' @return A data frame: `step`, `rows` written and `seconds`.
#' @export
derive_dataset <- function(
  results_root,
  db_path,
  dataset_id,
  level = c("all", "dataset", "fits"),
  fit_keys = NULL,
  artifact_base = db_path
) {
  level <- match.arg(level)
  con <- open_stability_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ctx <- derive_context(results_root, con, db_path, dataset_id, fit_keys, artifact_base)
  if (nrow(ctx$dataset) != 1L) {
    cli::cli_abort(
      "Dataset {.val {dataset_id}} is not in {.file {db_path}}; run {.fn load_dataset} first.",
      class = "sepsisfactorsdb_unmapped_key"
    )
  }
  dataset_steps <- list(
    genes = derive_genes,
    sample_metadata = derive_sample_metadata,
    gene_variance = derive_gene_variance
  )
  fit_steps <- list(
    factor_top_genes = derive_factor_top_genes,
    fit_scores = derive_fit_scores,
    fit_diag = derive_fit_diag,
    wgcna_dendrograms = derive_wgcna_dendrograms,
    driver_genes = derive_driver_genes,
    fit_summary = derive_fit_summary,
    factor_association = derive_factor_association,
    method_agreement = derive_method_agreement
  )
  steps <- switch(level,
    all = c(dataset_steps[1:2], fit_steps[1:5], dataset_steps[3], fit_steps[6:8]),
    dataset = dataset_steps,
    fits = fit_steps
  )
  if (level == "fits") {
    ctx$genes <- load_gene_lookup(ctx)
  }
  out <- data.frame(step = names(steps), rows = NA_integer_, seconds = NA_real_)
  with_transaction(con, {
    for (i in seq_along(steps)) {
      t0 <- proc.time()[["elapsed"]]
      out$rows[i] <- as.integer(steps[[i]](ctx))
      out$seconds[i] <- proc.time()[["elapsed"]] - t0
      # the later steps refer to genes by the numbers the genes step has just written
      if (names(steps)[i] == "genes") ctx$genes <- load_gene_lookup(ctx)
    }
  })
  out
}

#' Read the gene numbering of a dataset from the database
#'
#' @param ctx A derive context; the dataset's `genes` rows must be written.
#' @return A list: `lookup` (gene number named by feature id) and `by_symbol` (named by symbol).
#' @noRd
load_gene_lookup <- function(ctx) {
  g <- DBI::dbGetQuery(
    ctx$con,
    "SELECT gene_idx, gene_id, symbol FROM genes WHERE dataset_id = ?",
    params = list(ctx$dataset_id)
  )
  has_symbol <- !is.na(g$symbol)
  list(
    lookup = stats::setNames(g$gene_idx, g$gene_id),
    by_symbol = stats::setNames(g$gene_idx[has_symbol], g$symbol[has_symbol])
  )
}

#' Build the derived tables that span datasets
#'
#' Run after [load_cross_dataset()]. Stores each projection result as a
#' `projection_values` blob (components x samples weights, p values, then
#' per-sample r squared). A file without `pval` or `r_squared` stores `NA`.
#' The projection files are read from the database's tree, where
#' [load_cross_dataset()] placed them.
#'
#' @param db_path Path to the SQLite file.
#' @param projections The projections to store, a data frame with
#'   `projection_id` and `path`; `NULL` for every row of `projections`.
#' @param out_path The SQLite file to write the rows to; `db_path` itself when
#'   `NULL`. A parallel build writes one part file per source dataset.
#' @inheritParams load_dataset
#' @return A data frame: `step`, `rows` and `seconds`.
#' @export
derive_cross_dataset <- function(
  db_path,
  projections = NULL,
  out_path = NULL,
  artifact_base = db_path
) {
  t0 <- proc.time()[["elapsed"]]
  if (is.null(projections)) {
    con <- open_stability_db(db_path)
    projections <- DBI::dbGetQuery(con, "SELECT projection_id, path FROM projections")
    DBI::dbDisconnect(con)
    replace <- TRUE
  } else {
    replace <- FALSE
  }
  con <- open_stability_db(out_path %||% db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # a part file holds only these rows; the foreign keys are checked after the merge
  if (!is.null(out_path)) DBI::dbExecute(con, "PRAGMA foreign_keys = OFF")
  rows <- 0L
  with_transaction(con, {
    if (replace) {
      DBI::dbExecute(con, "DELETE FROM projection_values")
    }
    for (chunk in split(seq_len(nrow(projections)), ceiling(seq_len(nrow(projections)) / 200))) {
      cols <- lapply(chunk, function(i) {
        file <- resolve_artifact(projections$path[i], artifact_base)
        if (!file.exists(file)) {
          cli::cli_abort(
            "Projection file {.file {projections$path[i]}} is missing from the database tree."
          )
        }
        encode_projection(readRDS(file), projections$path[i])
      })
      rows <- rows + insert_columns(
        con,
        "projection_values",
        list(
          projection_id = projections$projection_id[chunk],
          row_ids = vapply(cols, `[[`, "", "row_ids"),
          col_ids = vapply(cols, `[[`, "", "col_ids"),
          data = lapply(cols, `[[`, "data")
        )
      )
    }
  })
  data.frame(step = "projection_values", rows = rows, seconds = proc.time()[["elapsed"]] - t0)
}

#' Pack a projection result for `projection_values`
#'
#' @param x A projection file's contents (`projection`, `pval`, `r_squared`).
#' @param path The file, for messages.
#' @noRd
encode_projection <- function(x, path) {
  p <- x$projection
  if (!is.matrix(p) || is.null(rownames(p)) || is.null(colnames(p))) {
    cli::cli_abort("{.file {path}} has no {.field projection} matrix with row and column names.")
  }
  pval <- if (is.null(x$pval)) matrix(NA_real_, nrow(p), ncol(p)) else x$pval
  r2 <- if (is.null(x$r_squared)) {
    rep(NA_real_, ncol(p))
  } else if (!is.null(names(x$r_squared))) {
    x$r_squared[colnames(p)]
  } else {
    x$r_squared
  }
  list(
    row_ids = json_ids(rownames(p)),
    col_ids = json_ids(colnames(p)),
    data = encode_f32(c(p, pval, r2))
  )
}
