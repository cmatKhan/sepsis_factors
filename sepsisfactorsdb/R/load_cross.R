#' Load the cross-dataset tables into the database
#'
#' Run after every dataset is loaded, since projections and gene-space
#' agreement link fits of two datasets. Replaces `projections`,
#' `gene_space_agreement`, `time_gene_recurrence` and `composition_markers`
#' with the contents of `cross_dataset/tables/`, places each projection result in its source
#' dataset's artifact directory, then removes artifact files in the database
#' tree that no row references. The projection values are stored by
#' [derive_cross_dataset()]. Does nothing if the results have no
#' `cross_dataset/` tables.
#'
#' @inheritParams load_dataset
#' @return `NULL` (invisibly) if there were no cross-dataset tables, otherwise
#'   a list: `n_projections`, `n_gene_space`, `orphans_removed` (per dataset),
#'   `written_at`.
#' @export
load_cross_dataset <- function(
  results_root,
  db_path,
  artifact_mode = "link",
  artifact_base = db_path
) {
  cdir <- results_cross_dir(results_root)
  tdir <- file.path(cdir, "tables")
  if (!dir.exists(tdir)) {
    return(invisible(NULL))
  }
  files <- list_table_files(tdir)
  tab <- function(name) if (name %in% names(files)) read_result_table(files[[name]]) else NULL

  con <- open_stability_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  fits <- DBI::dbGetQuery(con, "SELECT fit_id, fit_key, dataset_id FROM fits")
  fit_ids <- stats::setNames(fits$fit_id, fits$fit_key)
  fid <- function(keys) map_ids(keys, fit_ids, "fit")

  pr <- tab("projections")
  gs <- tab("gene_space_agreement")
  tg <- tab("time_gene_recurrence")
  cm <- tab("composition_markers")

  if (!is.null(pr) && nrow(pr)) {
    loaded <- DBI::dbGetQuery(con, "SELECT dataset_id FROM datasets")$dataset_id
    unknown <- setdiff(unique(pr$target_dataset_id), loaded)
    if (length(unknown)) {
      cli::cli_abort(
        c(
          paste0(
            "Projections target {cli::qty(length(unknown))}dataset{?s} ",
            "{.val {unknown}}, which {?is/are} not in the database."
          ),
          "i" = "Load every dataset the cross-dataset tables refer to first."
        ),
        class = "sepsisfactorsdb_unmapped_key"
      )
    }
    source_ds <- unname(stats::setNames(fits$dataset_id, fits$fit_key)[pr$source_fit_key])
    if (anyNA(source_ds)) fid(pr$source_fit_key) # raises the unmapped-key error
    place_artifacts(
      file.path(cdir, "artifacts", pr$path),
      file.path(dirname(artifact_base), artifact_rel(source_ds, pr$path)),
      artifact_mode
    )
    pr$path <- artifact_rel(source_ds, pr$path)
  }

  with_transaction(con, {
    DBI::dbExecute(con, "DELETE FROM projections")
    DBI::dbExecute(con, "DELETE FROM gene_space_agreement")
    DBI::dbExecute(con, "DELETE FROM time_gene_recurrence")
    DBI::dbExecute(con, "DELETE FROM composition_markers")
    append_rows(con, "time_gene_recurrence", tg)
    append_rows(con, "composition_markers", cm)
    if (!is.null(pr) && nrow(pr)) {
      append_rows(
        con,
        "projections",
        cbind(
          data.frame(source_fit_id = fid(pr$source_fit_key)),
          db_columns(pr, "projections")
        )
      )
    }
    if (!is.null(gs) && nrow(gs)) {
      append_rows(
        con,
        "gene_space_agreement",
        data.frame(
          source_fit_id = fid(gs$source_fit_key),
          target_fit_id = fid(gs$target_fit_key),
          n_genes_matched = gs$n_genes_matched,
          mean_abs_diagonal = gs$mean_abs_diagonal
        )
      )
    }
  })

  removed <- vapply(
    DBI::dbGetQuery(con, "SELECT dataset_id FROM datasets")$dataset_id,
    function(d) remove_orphan_artifacts(con, artifact_base, d),
    integer(1)
  )
  list(
    n_projections = if (is.null(pr)) 0L else nrow(pr),
    n_gene_space = if (is.null(gs)) 0L else nrow(gs),
    orphans_removed = removed,
    written_at = utc_now()
  )
}
