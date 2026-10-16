#' A dataset's artifact directory
#'
#' @param db_path Path to the SQLite file.
#' @param dataset_id Dataset id.
#' @return `<db dir>/stability_artifacts/<dataset_id>`.
#' @export
artifacts_dir <- function(db_path, dataset_id) {
  file.path(dirname(db_path), "stability_artifacts", dataset_id)
}

#' An artifact's path as the database stores it
#'
#' @param dataset_id Dataset id.
#' @param fname File name.
#' @return `stability_artifacts/<dataset_id>/<fname>`, relative to the
#'   database's directory.
#' @export
artifact_rel <- function(dataset_id, fname) {
  file.path("stability_artifacts", dataset_id, fname)
}

#' Resolve stored artifact paths
#'
#' @param path Paths as stored in the database (relative to its directory);
#'   absolute paths pass through.
#' @param db_path Path to the SQLite file.
#' @return Absolute paths; `NA` for `NA` or empty input.
#' @export
resolve_artifact <- function(path, db_path) {
  out <- rep(NA_character_, length(path))
  ok <- !is.na(path) & nzchar(path)
  is_abs <- ok & startsWith(path, "/")
  out[is_abs] <- path[is_abs]
  rel <- ok & !is_abs
  out[rel] <- file.path(normalizePath(dirname(db_path)), path[rel])
  out
}

#' Place artifact files in the database's directory tree
#'
#' @param from Existing source files.
#' @param to Destination paths (same length as `from`).
#' @param mode `"link"` hard-links and falls back to copying where a link is
#'   impossible (another filesystem); `"copy"` copies; `"move"` renames
#'   (falling back to copy and delete across filesystems); `"symlink"` makes
#'   an absolute symbolic link to the source, which must then stay in place.
#' @return `to`, invisibly.
#' @noRd
place_artifacts <- function(from, to, mode = c("link", "copy", "move", "symlink")) {
  mode <- match.arg(mode)
  if (length(from) != length(to)) {
    cli::cli_abort("{.arg from} and {.arg to} must have the same length.")
  }
  if (!length(from)) {
    return(invisible(to))
  }
  for (dir in unique(dirname(to))) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  same <- normalizePath(from, mustWork = TRUE) == suppressWarnings(normalizePath(to, mustWork = FALSE))
  from <- from[!same]
  to <- to[!same]
  if (!length(from)) {
    return(invisible(to))
  }
  unlink(to)
  done <- switch(mode,
    link = suppressWarnings(file.link(from, to)),
    symlink = suppressWarnings(file.symlink(normalizePath(from), to)),
    copy = file.copy(from, to, overwrite = TRUE),
    move = file.rename(from, to)
  )
  if (mode == "symlink" && !all(done)) {
    cli::cli_abort("Could not link {sum(!done)} artifact file{?s}, e.g. {.file {from[!done][1]}}.")
  }
  if (!all(done)) {
    left <- !done
    ok <- file.copy(from[left], to[left], overwrite = TRUE)
    if (!all(ok)) {
      cli::cli_abort("Could not place {sum(!ok)} artifact file{?s}, e.g. {.file {from[left][!ok][1]}}.")
    }
    if (mode == "move") unlink(from[left])
  }
  invisible(to)
}

#' Delete a dataset's unreferenced artifact files
#'
#' Files under the dataset's artifact directory in the database tree that no
#' row references (`fit_artifacts`, `pattern_drivers`, `projections`), e.g.
#' fits dropped from a grid or keys from an earlier load. Only the database's tree is touched, never the
#' results output.
#'
#' @param con DB connection.
#' @param db_path Path to the database.
#' @param dataset_id Dataset id.
#' @return Number of files deleted.
#' @export
remove_orphan_artifacts <- function(con, db_path, dataset_id) {
  ds <- list(dataset_id)
  q <- function(sql) unlist(DBI::dbGetQuery(con, sql, params = ds), use.names = FALSE)
  in_ds <- "(SELECT fit_id FROM fits WHERE dataset_id = ?)"
  referenced <- c(
    q(sprintf("SELECT path FROM fit_artifacts WHERE fit_id IN %s", in_ds)),
    q(sprintf(
      "SELECT d.path FROM pattern_drivers d JOIN factors x ON x.factor_id = d.factor_id
       WHERE x.fit_id IN %s",
      in_ds
    )),
    q(sprintf("SELECT path FROM projections WHERE source_fit_id IN %s", in_ds))
  )
  prefix <- paste0("stability_artifacts/", dataset_id, "/")
  referenced <- sub(prefix, "", referenced[!is.na(referenced)], fixed = TRUE)
  dir <- artifacts_dir(db_path, dataset_id)
  orphans <- setdiff(list.files(dir, recursive = TRUE), referenced)
  unlink(file.path(dir, orphans))
  length(orphans)
}
