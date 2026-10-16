# Datasets, methods and genes: what the pages list and label.

DOCS_URL <- "https://cmatkhan.github.io/sepsis_latent_factor_eda/"
PUBLIC_COLLECTION <- "cmatkhan/Sepsis transcriptomics"

list_datasets <- function(db) {
  db_query(db, "SELECT dataset_id FROM datasets ORDER BY dataset_id")$dataset_id
}

# One row per dataset: how to read its metadata, family and documentation collection.
datasets_info <- function(db) {
  db_query(
    db,
    "SELECT dataset_id, sample_id_col, feature_id_col, symbol_col, subject_id_col, family,
            family_position, hf_collection
     FROM datasets ORDER BY dataset_id"
  )
}

# The subject id column of a dataset (NA when it has none).
dataset_subject_col <- function(db, dataset_id) {
  col <- db_query(
    db, "SELECT subject_id_col FROM datasets WHERE dataset_id = ?",
    params = list(dataset_id)
  )$subject_id_col
  if (length(col) == 1) col else NA_character_
}

## ---- methods ------------------------------------------------------------------------

# The methods in display order, with their label, colour, blurb and properties.
methods_table <- function(db) {
  db_query(
    db,
    "SELECT method, family, sign_ambiguous, has_loadings, label, color, display_order, blurb
     FROM methods ORDER BY display_order"
  )
}

# `m` in display order (methods of the table only).
order_methods <- function(info, m) {
  info$method[info$method %in% m]
}

method_label <- function(info, m) {
  out <- info$label[match(m, info$method)]
  unname(ifelse(is.na(out), m, out))
}

# Named vector method -> colour.
method_colors <- function(info) {
  stats::setNames(info$color, info$method)
}

# Whether a method has loadings (WGCNA has modules instead).
method_has_loadings <- function(info, m) {
  info$has_loadings[match(m, info$method)] == 1
}

# Methods with at least one ok fit for a dataset, in display order.
methods_for_dataset <- function(db, info, dataset_id) {
  m <- db_query(
    db,
    "SELECT DISTINCT method FROM fits WHERE dataset_id = ? AND status = 'ok'",
    params = list(dataset_id)
  )$method
  order_methods(info, m)
}

# Fit counts per method for one dataset (v_ingest_summary).
fit_coverage <- function(db, dataset_id) {
  db_query(
    db,
    "SELECT method, family, n_fits, n_ok, n_failed, n_representative
     FROM v_ingest_summary WHERE dataset_id = ?",
    params = list(dataset_id)
  )
}

## ---- dataset families -----------------------------------------------------------------

# Timepoint order for dataset ids of one family: by `family_position` when every id has
# one, else by the last number in the id (PRE first; DISCHARGE and POST last), else
# alphabetical. Returns a permutation.
order_timepoints <- function(ids, position = NULL) {
  if (!is.null(position) && !anyNA(position)) {
    return(order(position, ids))
  }
  key <- rep(NA_real_, length(ids))
  key[grepl("PRE$", ids, ignore.case = TRUE)] <- -Inf
  key[is.na(key) & grepl("DISCHARGE$|POST", ids, ignore.case = TRUE)] <- Inf
  need <- is.na(key)
  last_number <- function(s) {
    m <- regmatches(s, gregexpr("\\d+", s))[[1]]
    if (length(m)) as.numeric(m[length(m)]) else NA_real_
  }
  key[need] <- vapply(ids[need], last_number, numeric(1))
  order(key, ids, na.last = TRUE)
}

# The family positions of dataset ids, NA where the database has none.
family_positions <- function(db, ids) {
  info <- datasets_info(db)
  info$family_position[match(ids, info$dataset_id)]
}

# The docs page of a dataset's family, or NULL when its collection is not the public one.
dataset_docs_url <- function(db, dataset_id, parent_id = dataset_id) {
  info <- datasets_info(db)
  col <- stats::setNames(info$hf_collection, info$dataset_id)
  public <- function(id) isTRUE(unname(col[id]) == PUBLIC_COLLECTION)
  if (!public(dataset_id) && !public(parent_id)) {
    return(NULL)
  }
  paste0(DOCS_URL, "datasets.html#", tolower(gsub("[^A-Za-z0-9]+", "-", parent_id)))
}

# The marker genes behind the composition scores, as
# list(neutrophil = ..., lymphocyte = ...), or NULL (logged) when the database has none.
composition_markers <- function(db) {
  d <- db_query(db, "SELECT marker_set, gene FROM composition_markers ORDER BY marker_set, gene")
  if (nrow(d) == 0) {
    note_missing("composition marker list", "the database has no composition_markers")
    return(NULL)
  }
  split(d$gene, d$marker_set)
}

## ---- genes -----------------------------------------------------------------------------

# Id, symbol and number of every gene of a dataset.
dataset_genes <- function(db, dataset_id) {
  db_query(
    db,
    "SELECT gene_idx, gene_id, symbol FROM genes WHERE dataset_id = ? ORDER BY gene_idx",
    params = list(dataset_id)
  )
}

# Named vector feature id -> display label (the symbol, else the id itself).
gene_labels <- function(db, dataset_id) {
  g <- dataset_genes(db, dataset_id)
  stats::setNames(ifelse(is.na(g$symbol) | !nzchar(g$symbol), g$gene_id, g$symbol), g$gene_id)
}

label_genes <- function(labels, ids) {
  out <- unname(labels[ids])
  ifelse(is.na(out), ids, out)
}
