# Queries behind the variance pages (the repeated-measures tables). A missing table is a
# notice (if_missing()), never an error.

VARIANCE_TABLES <- c("dataset_design", "variance_summary", "variance_categories", "variance_terms")

variance_ready <- function(db) {
  miss <- db_missing_tables(db, VARIANCE_TABLES)
  if (length(miss) > 0) {
    note_missing(
      "repeated-measures tables",
      paste("absent:", paste(miss, collapse = ", "))
    )
    return(FALSE)
  }
  TRUE
}

design_table <- function(db) {
  if_missing(db_query(db, "SELECT * FROM dataset_design"), "dataset_design")
}

variance_summary_table <- function(db) {
  if_missing(db_query(db, "SELECT * FROM variance_summary"), "variance_summary")
}

# Share of variance per category and dataset for the chosen gene set / weighting (a
# column of `variance_categories`); only categories in the dataset's model.
variance_long <- function(db, datasets, column) {
  column <- match.arg(column, unname(VARIANCE_GENESETS))
  d <- if_missing(
    db_query(
      db,
      "SELECT dataset_id, category, present, vw_factor_genes, vw_all, vw_top, median
       FROM variance_categories"
    ),
    "variance_categories"
  )
  if (is.null(d)) {
    return(NULL)
  }
  d$share <- d[[column]]
  d <- d[d$present == 1 & d$dataset_id %in% datasets, ]
  d <- d[c("dataset_id", "category", "present", "share")]
  d$category <- factor(d$category, levels = names(VARIANCE_COLORS))
  d
}

variance_terms_for <- function(db, dataset_id) {
  if_missing(
    db_query(db, "SELECT * FROM variance_terms WHERE dataset_id = ?", list(dataset_id)),
    "variance_terms"
  )
}

variance_bins_for <- function(db, dataset_id) {
  if_missing(
    db_query(db, "SELECT * FROM variance_bins WHERE dataset_id = ?", list(dataset_id)),
    "variance_bins"
  )
}

time_genes_for <- function(db, dataset_id, n = 25) {
  if_missing(
    db_query(
      db,
      "SELECT symbol, time_all, between_subject FROM variance_time_genes
       WHERE dataset_id = ? ORDER BY time_all DESC LIMIT ?",
      list(dataset_id, n)
    ),
    "variance_time_genes"
  )
}

time_gene_recurrence <- function(db, min_datasets = 2) {
  if_missing(
    db_query(
      db,
      "SELECT symbol, n_datasets, datasets FROM time_gene_recurrence WHERE n_datasets >= ?
       ORDER BY n_datasets DESC, symbol",
      list(min_datasets)
    ),
    "time_gene_recurrence"
  )
}

samples_for <- function(db, dataset_id) {
  if_missing(
    db_query(db, "SELECT * FROM samples WHERE dataset_id = ?", list(dataset_id)),
    "samples"
  )
}

trait_variance_for <- function(db, dataset_id) {
  if_missing(
    db_query(db, "SELECT * FROM trait_variance WHERE dataset_id = ?", list(dataset_id)),
    "trait_variance"
  )
}

composition_correlations_for <- function(db, dataset_id) {
  if_missing(
    db_query(db, "SELECT * FROM composition_correlations WHERE dataset_id = ?", list(dataset_id)),
    "composition_correlations"
  )
}

de_contrasts_for <- function(db, dataset_id) {
  if_missing(
    db_query(
      db,
      "SELECT contrast, kind, readable, n_genes, n_fdr05 FROM de_contrasts WHERE dataset_id = ?",
      list(dataset_id)
    ),
    "de_contrasts"
  )
}

# PC1's Spearman correlation with each leukocyte marker score, one row per dataset
# (stored once per pair, in either orientation).
pc1_composition_table <- function(db) {
  scores <- c("neutrophil_score", "lymphocyte_score", "nl_score")
  d <- if_missing(
    db_query(
      db,
      "SELECT dataset_id, x, y, rho, p_value FROM composition_correlations
       WHERE (x = 'PC1' AND y IN (?, ?, ?)) OR (y = 'PC1' AND x IN (?, ?, ?))",
      as.list(c(scores, scores))
    ),
    "composition_correlations"
  )
  if (is.null(d) || nrow(d) == 0) {
    return(NULL)
  }
  d$score <- ifelse(d$x == "PC1", d$y, d$x)
  w <- stats::reshape(
    d[, c("dataset_id", "score", "rho", "p_value")],
    idvar = "dataset_id", timevar = "score", direction = "wide"
  )
  col <- function(nm, v) {
    if (paste0(v, ".", nm) %in% names(w)) w[[paste0(v, ".", nm)]] else rep(NA_real_, nrow(w))
  }
  data.frame(
    dataset = w$dataset_id, neutrophil = col("neutrophil_score", "rho"),
    lymphocyte = col("lymphocyte_score", "rho"), nl = col("nl_score", "rho"),
    p_nl = col("nl_score", "p_value"), stringsAsFactors = FALSE
  )
}
