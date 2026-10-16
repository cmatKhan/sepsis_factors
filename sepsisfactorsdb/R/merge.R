# Merging part files into one database (see ingest_parallel()).

#' Surrogate ids and the foreign keys that refer to them
#'
#' Read from the schema (`PRAGMA table_info` and `foreign_key_list`), so the merge
#' knows no table by name. A surrogate id is a table's single `INTEGER PRIMARY
#' KEY` column that is not itself a foreign key; a foreign key is *shifted* when
#' it refers to such a column.
#'
#' @param con A connection to a database with the schema.
#' @return A list: `tables` (in an order where a table follows those it refers
#'   to), `columns` (per table, its columns), `surrogate` (per table, its
#'   surrogate id column or `NULL`) and `shifted` (a data frame: `table`, `from`,
#'   `ref_table`).
#' @noRd
id_structure <- function(con) {
  tabs <- DBI::dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%'")$name
  info <- lapply(stats::setNames(tabs, tabs), function(t) DBI::dbGetQuery(con, sprintf("PRAGMA table_info(%s)", t)))
  fks <- do.call(rbind, lapply(tabs, function(t) {
    f <- DBI::dbGetQuery(con, sprintf("PRAGMA foreign_key_list(%s)", t))
    if (nrow(f)) data.frame(table = t, from = f$from, ref_table = f$table, to = f$to) else NULL
  }))
  if (is.null(fks)) fks <- data.frame(table = character(), from = character(), ref_table = character(), to = character())
  surrogate <- lapply(info, function(i) {
    pk <- i$name[i$pk > 0]
    if (length(pk) == 1L && toupper(i$type[i$pk > 0]) == "INTEGER") pk else NULL
  })
  is_own <- vapply(tabs, function(t) !is.null(surrogate[[t]]) && !surrogate[[t]] %in% fks$from[fks$table == t], logical(1))
  surrogate[!is_own] <- list(NULL)
  shifted <- fks[mapply(function(rt, to) is_own[[rt]] && identical(surrogate[[rt]], to), fks$ref_table, fks$to), , drop = FALSE]
  # order: a table after the tables it refers to
  ordered <- character()
  while (length(ordered) < length(tabs)) {
    todo <- setdiff(tabs, ordered)
    ready <- todo[vapply(todo, function(t) all(setdiff(fks$ref_table[fks$table == t], t) %in% ordered), logical(1))]
    if (!length(ready)) cli::cli_abort("The schema has a cycle of foreign keys.")
    ordered <- c(ordered, ready)
  }
  list(
    tables = ordered,
    columns = lapply(info, function(i) i$name),
    surrogate = surrogate,
    own = names(is_own)[is_own],
    shifted = shifted
  )
}

#' Highest id of every table that has a surrogate id
#'
#' @noRd
max_ids <- function(con, s) {
  stats::setNames(
    vapply(s$own, function(t) {
      as.numeric(DBI::dbGetQuery(con, sprintf("SELECT COALESCE(MAX(%s), 0) AS m FROM main.%s", s$surrogate[[t]], t))$m)
    }, numeric(1)),
    s$own
  )
}

#' Add the rows of the parts of one unit to the merged database
#'
#' @param con Connection to the merged database.
#' @param parts Paths of the unit's parts, the load part first.
#' @param s `id_structure()` of the merged database.
#' @param renumber Shift the ids (parts of a parallel build have ids from 1).
#' @noRd
merge_unit <- function(con, parts, s, renumber) {
  # The unit's foreign keys are shifted by the highest ids before its first part.
  # A table's own ids are shifted by the highest id so far, which differs from
  # that for a table the unit's later parts add rows to (method_agreement).
  frozen <- max_ids(con, s)
  for (part in parts) {
    DBI::dbExecute(con, sprintf("ATTACH DATABASE %s AS part", DBI::dbQuoteString(con, part)))
    DBI::dbExecute(con, "BEGIN")
    current <- max_ids(con, s)
    for (t in setdiff(s$tables, "meta")) {
      cols <- s$columns[[t]]
      expr <- as.character(DBI::dbQuoteIdentifier(con, cols))
      if (renumber) {
        own <- s$surrogate[[t]]
        fk <- s$shifted[s$shifted$table == t, , drop = FALSE]
        for (k in seq_along(cols)) {
          shift <- if (identical(cols[k], own)) {
            current[[t]]
          } else if (cols[k] %in% fk$from) {
            frozen[[fk$ref_table[fk$from == cols[k]][1]]]
          } else {
            0
          }
          if (shift != 0) expr[k] <- sprintf("%s + %s", expr[k], format(shift, scientific = FALSE))
        }
      }
      verb <- if (t == "methods") "INSERT OR IGNORE" else "INSERT"
      DBI::dbExecute(
        con,
        sprintf(
          "%s INTO main.%s (%s) SELECT %s FROM part.%s",
          verb, t, paste(DBI::dbQuoteIdentifier(con, cols), collapse = ", "), paste(expr, collapse = ", "), t
        )
      )
    }
    DBI::dbExecute(con, "COMMIT")
    DBI::dbExecute(con, "DETACH DATABASE part")
  }
  invisible(NULL)
}

#' Merge part files into one database
#'
#' Combines the part files of a parallel build ([ingest_parallel()]) with SQLite's
#' own `ATTACH` and `INSERT ... SELECT`, table by table in foreign-key order. The
#' merged database is made in `merge_dir` (local disk), without secondary indexes,
#' which are created after the rows are in; with `db_path` it is then checked
#' ([check_database()]) and copied to `db_path` under a temporary name and renamed,
#' so a failed merge never replaces the old database.
#'
#' @section Ids: Every part is built on its own, so every part numbers its rows
#'   from 1: `fits.fit_id` is 1 in each dataset's part, and the same for
#'   `factor_id`, `fit_pair_id`, `match_id`, `query_id`, `driver_id`,
#'   `projection_id` and `agreement_id`. The merge **renumbers** them. It needs no
#'   knowledge of the tables: from the schema it finds each table's surrogate id
#'   (its single `INTEGER PRIMARY KEY` that is not a foreign key) and every foreign
#'   key that refers to one. Parts are merged in a fixed order: `units` (a dataset's
#'   load part, followed straight away by its chunk parts), in the order given,
#'   which the pipeline sorts by dataset id. Before a unit, the merge records the
#'   highest id of each table already in the merged file, its *offset*. In the
#'   unit's parts each surrogate id gets the offset of its own table added and each
#'   foreign key the offset of the table it refers to. The chunk parts of a dataset
#'   reuse the ids of the dataset's load part (they refer to its fits, factors and
#'   drivers), so they are shifted by the same offsets; the only ids a chunk part
#'   owns itself (`agreement_id`) are shifted by the highest id so far, so chunks
#'   do not collide. Because the order is fixed, the result is identical, ids
#'   included, to a serial [ingest_results()] of the same datasets (which loads
#'   them sorted by id as well).
#'
#'   What is and is not stable: ids are stable for the same results and the same
#'   order, but they **shift** when an earlier dataset changes in size or a dataset
#'   is added before others. Nothing may rely on an id staying the same between
#'   builds: artifact files are named by the fit key, and results refer to fits and
#'   factors by natural keys. The surrogate ids that cross-dataset tables hold
#'   (`projections.source_fit_id`, `gene_space_agreement`) are resolved after the
#'   dataset merge, from the merged `fits`. The ids that other tables hold are
#'   renumbered, so a reader must join, never compare ids across builds.
#'
#' @param part_paths Paths of the part files, in merge order.
#' @param db_path Where to publish the merged database; `NULL` to leave it in
#'   `merge_dir` (returned) for further steps.
#' @param merge_dir Directory for the merged file while it is built (local disk).
#' @param units Which parts form a unit: a vector as long as `part_paths`, equal
#'   values for the parts of one unit, which must be adjacent. Default: every part
#'   its own unit.
#' @param renumber `FALSE` for parts whose ids are already final, such as
#'   the projection-value parts, built against the merged database.
#' @param into An existing merged file (from an earlier call with `db_path = NULL`)
#'   to add the parts to, instead of a new one.
#' @return `db_path` if given, otherwise the path of the merged file, invisibly.
#' @export
merge_parts <- function(part_paths, db_path = NULL, merge_dir = tempfile("merge-"), units = seq_along(part_paths), renumber = TRUE, into = NULL) {
  stopifnot(length(units) == length(part_paths))
  missing <- part_paths[!file.exists(part_paths)]
  if (length(missing)) {
    cli::cli_abort("Part file{?s} {.file {missing}} do{?es/} not exist.")
  }
  if (is.null(into)) {
    dir.create(merge_dir, recursive = TRUE, showWarnings = FALSE)
    merged <- file.path(merge_dir, "merged.sqlite")
    unlink(c(merged, paste0(merged, c("-wal", "-shm"))))
    con <- open_db(merged, indexes = FALSE)
    DBI::dbExecute(con, "PRAGMA journal_mode = OFF")
  } else {
    merged <- into
    con <- open_db(merged, indexes = TRUE)
    DBI::dbExecute(con, "PRAGMA journal_mode = OFF")
  }
  ok <- FALSE
  on.exit({
    DBI::dbDisconnect(con)
    if (!ok && is.null(into)) unlink(merged)
  })
  DBI::dbExecute(con, "PRAGMA synchronous = OFF")
  DBI::dbExecute(con, "PRAGMA foreign_keys = OFF")
  s <- id_structure(con)
  for (u in unique(units)) {
    idx <- which(units == u)
    if (!identical(idx, seq(min(idx), max(idx)))) {
      cli::cli_abort("The parts of unit {.val {u}} are not adjacent in {.arg part_paths}.")
    }
    merge_unit(con, part_paths[idx], s, renumber)
  }
  if (is.null(into)) {
    create_indexes(con)
  }
  ok <- TRUE
  DBI::dbDisconnect(con)
  on.exit()
  if (is.null(db_path)) {
    return(invisible(merged))
  }
  publish_database(merged, db_path)
}

#' Check a database and copy it into place
#'
#' Runs [check_database()] on `merged`, then copies it to `db_path` under a
#' temporary name and renames it, so the old database is replaced only by a
#' complete, valid one.
#'
#' @param merged The finished database file (on local disk).
#' @param db_path Where it goes.
#' @return `db_path`, invisibly.
#' @noRd
publish_database <- function(merged, db_path) {
  check_database(merged)
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(db_path, ".new", Sys.getpid())
  on.exit(unlink(tmp))
  if (!file.copy(merged, tmp, overwrite = TRUE)) {
    cli::cli_abort("Could not copy the merged database to {.file {dirname(db_path)}}.")
  }
  unlink(paste0(db_path, c("-wal", "-shm")))
  if (!file.rename(tmp, db_path)) {
    cli::cli_abort("Could not replace {.file {db_path}}.")
  }
  invisible(db_path)
}

#' Check a database
#'
#' Foreign keys and SQLite's integrity check must both come back empty. The file
#' is left in rollback-journal mode (no `-wal` file), as the merge and the parts
#' write it, which a read-only mount can open.
#'
#' @param path The database file.
#' @return `path`, invisibly; an error naming the first violations otherwise.
#' @export
check_database <- function(path) {
  con <- open_stability_db(path)
  on.exit(DBI::dbDisconnect(con))
  fk <- DBI::dbGetQuery(con, "PRAGMA foreign_key_check")
  if (nrow(fk)) {
    cli::cli_abort(
      c(
        "{nrow(fk)} foreign key violation{?s} in {.file {path}}.",
        "i" = "First: table {.val {fk$table[1]}}, row {fk$rowid[1]}, refers to {.val {fk$parent[1]}}."
      ),
      class = "sepsisfactorsdb_invalid_database"
    )
  }
  ic <- DBI::dbGetQuery(con, "PRAGMA integrity_check")[[1]]
  if (!identical(ic, "ok")) {
    cli::cli_abort(c("Integrity check of {.file {path}} failed.", "x" = "{ic[1]}"), class = "sepsisfactorsdb_invalid_database")
  }
  DBI::dbExecute(con, "PRAGMA wal_checkpoint(TRUNCATE)")
  DBI::dbExecute(con, "PRAGMA journal_mode = DELETE")
  invisible(path)
}
