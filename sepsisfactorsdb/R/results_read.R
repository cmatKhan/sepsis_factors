results_dataset_dir <- function(results_root, dataset_id) {
  file.path(results_root, "datasets", dataset_id)
}

results_cross_dir <- function(results_root) file.path(results_root, "cross_dataset")

invalid_results <- function(message, ..., .envir = parent.frame()) {
  cli::cli_abort(message, ..., class = "sepsisfactorsdb_invalid_results", .envir = .envir)
}

#' Read a results-output manifest
#'
#' @param results_root The results directory.
#' @return A list with `results_output_version`, `writer`, `created_at`,
#'   `datasets` (character vector) and `methods` (named list of
#'   `method_output_version`).
#' @export
read_manifest <- function(results_root) {
  path <- file.path(results_root, "manifest.json")
  if (!file.exists(path)) {
    invalid_results("No {.file manifest.json} in {.file {results_root}}; is this a results output?")
  }
  manifest <- jsonlite::read_json(path, simplifyVector = TRUE)
  needed <- c("results_output_version", "datasets", "methods")
  missing <- setdiff(needed, names(manifest))
  if (length(missing)) {
    invalid_results("{.file {path}} lacks {.field {missing}}.")
  }
  manifest$datasets <- as.character(manifest$datasets)
  manifest
}

#' Read one results-output table
#'
#' @param path Path to a `.parquet` file.
#' @param cols Optional columns to read.
#' @return A plain data frame.
#' @noRd
read_result_table <- function(path, cols = NULL) {
  df <- if (is.null(cols)) {
    arrow::read_parquet(path)
  } else {
    arrow::read_parquet(path, col_select = arrow::all_of(cols))
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  }
  df
}

#' The column names stored in a Parquet file
#'
#' @param path Path to a `.parquet` file.
#' @return Character vector (reads metadata only).
#' @noRd
parquet_columns <- function(path) {
  arrow::ParquetFileReader$create(path)$GetSchema()$names
}

#' The tables present in a tables directory
#'
#' @param tables_dir `.../tables`.
#' @return Named character vector: table -> path.
#' @noRd
list_table_files <- function(tables_dir) {
  files <- list.files(tables_dir, pattern = "\\.parquet$", full.names = TRUE)
  stats::setNames(files, sub("\\.parquet$", "", basename(files)))
}

#' Check one table file's columns against the contract
#'
#' @noRd
check_table_columns <- function(table, path, scope) {
  spec <- RESULT_TABLES[[table]]
  if (is.null(spec) || spec$scope != scope) {
    invalid_results("{.file {path}} is not a {scope}-level results table ({.val {table}}).")
  }
  have <- parquet_columns(path)
  required <- contract_columns(table)
  optional <- setdiff(contract_columns(table, include_optional = TRUE), required)
  missing <- setdiff(required, have)
  extra <- setdiff(have, c(required, optional))
  if (length(missing) || length(extra)) {
    invalid_results(c(
      "Table {.val {table}} in {.file {path}} does not match the results-output contract.",
      "x" = if (length(missing)) "Missing column{?s}: {.field {missing}}",
      "x" = if (length(extra)) "Unexpected column{?s}: {.field {extra}}"
    ))
  }
  invisible(have)
}

#' Check a dataset's results against the contract
#'
#' @noRd
validate_dataset_results <- function(results_root, dataset_id, manifest) {
  ddir <- results_dataset_dir(results_root, dataset_id)
  tdir <- file.path(ddir, "tables")
  if (!dir.exists(tdir)) {
    invalid_results("Dataset {.val {dataset_id}} has no {.file tables/} in {.file {ddir}}.")
  }
  files <- list_table_files(tdir)
  for (table in names(files)) check_table_columns(table, files[[table]], "dataset")
  for (required in c("datasets", "methods", "fits")) {
    if (!required %in% names(files)) {
      invalid_results("Dataset {.val {dataset_id}} lacks the required table {.val {required}}.")
    }
  }

  datasets <- read_result_table(files[["datasets"]])
  if (nrow(datasets) != 1L || !identical(datasets$dataset_id, dataset_id)) {
    invalid_results("{.val datasets} of {.val {dataset_id}} must hold exactly its own row.")
  }
  for (table in setdiff(intersect(names(files), names(RESULT_TABLES)), "datasets")) {
    if ("dataset_id" %in% contract_columns(table)) {
      ids <- unique(read_result_table(files[[table]], "dataset_id")$dataset_id)
      if (length(ids) && !identical(ids, dataset_id)) {
        invalid_results(
          "Table {.val {table}} of {.val {dataset_id}} holds rows for {.val {setdiff(ids, dataset_id)}}."
        )
      }
    }
  }

  methods <- read_result_table(files[["methods"]])
  fits <- read_result_table(files[["fits"]])
  check_method_outputs(dataset_id, methods, fits, files, ddir, manifest)

  for (table in intersect(names(files), names(RESULT_TABLES))) {
    check_keys(dataset_id, table, files[[table]], fits, files)
  }
  check_artifact_files(dataset_id, files, ddir)
  invisible(list(files = files, methods = methods, fits = fits))
}

#' Check the methods in a dataset against the supported outputs
#'
#' @noRd
check_method_outputs <- function(dataset_id, methods, fits, files, ddir, manifest) {
  unknown <- setdiff(unique(c(methods$method, fits$method)), names(manifest$methods))
  if (length(unknown)) {
    invalid_results(
      "Dataset {.val {dataset_id}} uses {cli::qty(length(unknown))}method{?s} {.val {unknown}} not listed in the manifest."
    )
  }
  fit_artifacts <- if ("fit_artifacts" %in% names(files)) {
    read_result_table(files[["fit_artifacts"]])
  } else {
    data.frame(fit_key = character(), kind = character(), path = character())
  }
  for (m in unique(fits$method)) {
    spec <- SUPPORTED_METHOD_OUTPUTS[[m]]
    row <- methods[methods$method == m, , drop = FALSE]
    if (nrow(row) != 1L) {
      invalid_results("Dataset {.val {dataset_id}} has no single {.val methods} row for {.val {m}}.")
    }
    expected <- c(spec[c("family", "sign_ambiguous", "has_loadings")])
    actual <- list(
      family = row$family,
      sign_ambiguous = as.logical(row$sign_ambiguous),
      has_loadings = as.logical(row$has_loadings)
    )
    for (field in names(expected)) {
      if (!identical(as.vector(expected[[field]]), as.vector(actual[[field]]))) {
        invalid_results(
          "Method {.val {m}} in {.val {dataset_id}}: {.field {field}} is {.val {actual[[field]]}}, version {manifest$methods[[m]]$method_output_version} of its output requires {.val {expected[[field]]}}."
        )
      }
    }
    ok <- fits[fits$method == m & fits$status == "ok", , drop = FALSE]
    for (kind in spec$fit_artifacts) {
      need <- ok$fit_key
      if (kind == "scores" && isTRUE(spec$scores_unless_bootstrap)) {
        need <- ok$fit_key[ok$bootstrap == 0]
      }
      have <- fit_artifacts$fit_key[fit_artifacts$kind == kind]
      lacking <- setdiff(need, have)
      if (length(lacking)) {
        invalid_results(c(
          "{length(lacking)} ok {.val {m}} fit{cli::qty(length(lacking))}{?s} in {.val {dataset_id}} lack{cli::qty(length(lacking))}{?s/} a {.val {kind}} artifact.",
          "i" = "First: {.val {utils::head(lacking, 3)}}"
        ))
      }
    }
    if (nrow(ok)) {
      for (table in spec$tables) {
        if (!table %in% names(files)) {
          invalid_results(
            "Method {.val {m}} in {.val {dataset_id}} requires the table {.val {table}}."
          )
        }
      }
    }
  }
  invisible(NULL)
}

#' Check that natural keys in a table refer to fits (and factors) that exist
#'
#' @noRd
check_keys <- function(dataset_id, table, path, fits, files) {
  cols <- contract_columns(table)
  fit_cols <- intersect(
    c("fit_key", "fit_a_key", "fit_b_key", "centrotype_fit_key", "source_fit_key", "target_fit_key"),
    cols
  )
  if (table %in% c("projections", "gene_space_agreement")) {
    return(invisible(NULL))
  }
  for (col in fit_cols) {
    keys <- unique(stats::na.omit(read_result_table(path, col)[[col]]))
    unknown <- setdiff(keys, fits$fit_key)
    if (length(unknown)) {
      invalid_results(c(
        "Table {.val {table}} of {.val {dataset_id}}: {length(unknown)} {.field {col}} value{cli::qty(length(unknown))}{?s} not in {.val fits}.",
        "i" = "First: {.val {utils::head(unknown, 3)}}"
      ))
    }
  }
  invisible(NULL)
}

#' Check that every artifact a table references exists
#'
#' @noRd
check_artifact_files <- function(dataset_id, files, ddir) {
  for (table in intersect(names(files), names(RESULT_TABLES))) {
    for (col in RESULT_TABLES[[table]]$paths) {
      paths <- unique(stats::na.omit(read_result_table(files[[table]], col)[[col]]))
      missing <- paths[!file.exists(file.path(ddir, "artifacts", paths))]
      if (length(missing)) {
        invalid_results(c(
          "{length(missing)} artifact file{cli::qty(length(missing))}{?s} referenced by {.field {table}${col}} of {.val {dataset_id}} {cli::qty(length(missing))}{?is/are} missing.",
          "i" = "First: {.file {utils::head(missing, 3)}}"
        ))
      }
    }
  }
  invisible(NULL)
}

#' Check the cross-dataset tables
#'
#' @noRd
validate_cross_results <- function(results_root) {
  cdir <- results_cross_dir(results_root)
  tdir <- file.path(cdir, "tables")
  if (!dir.exists(tdir)) {
    return(invisible(NULL))
  }
  files <- list_table_files(tdir)
  for (table in names(files)) check_table_columns(table, files[[table]], "cross")
  for (table in names(files)) {
    for (col in RESULT_TABLES[[table]]$paths) {
      paths <- unique(stats::na.omit(read_result_table(files[[table]], col)[[col]]))
      missing <- paths[!file.exists(file.path(cdir, "artifacts", paths))]
      if (length(missing)) {
        invalid_results(c(
          "{length(missing)} artifact file{cli::qty(length(missing))}{?s} referenced by {.field {table}${col}} {cli::qty(length(missing))}{?is/are} missing.",
          "i" = "First: {.file {utils::head(missing, 3)}}"
        ))
      }
    }
  }
  invisible(files)
}

#' Validate a results output
#'
#' Checks everything that can be checked without writing: the manifest and its
#' [RESULTS_OUTPUT_VERSION]; that every method present is one this package can
#' load, at a `method_output_version` it supports ([supported_methods()]); each
#' table's columns against [contract_columns()]; that natural keys refer to
#' fits that exist; that each method's `methods` row and required artifacts
#' match its output spec; and that every referenced artifact file exists.
#'
#' @param results_root The results directory.
#' @param datasets Dataset ids to check; `NULL` for all in the manifest.
#' @return The manifest, invisibly. Errors of class
#'   `sepsisfactorsdb_invalid_results` (or
#'   `sepsisfactorsdb_results_version` / `sepsisfactorsdb_unsupported_method`)
#'   otherwise.
#' @export
validate_results <- function(results_root, datasets = NULL) {
  manifest <- read_manifest(results_root)
  if (!identical(as.integer(manifest$results_output_version), RESULTS_OUTPUT_VERSION)) {
    cli::cli_abort(
      c(
        "{.file {results_root}} has results-output version {manifest$results_output_version}, but this package loads version {RESULTS_OUTPUT_VERSION}.",
        "i" = "Update {.pkg sepsisfactors} or {.pkg sepsisfactorsdb} so that the versions agree."
      ),
      class = "sepsisfactorsdb_results_version"
    )
  }
  for (m in names(manifest$methods)) {
    spec <- SUPPORTED_METHOD_OUTPUTS[[m]]
    version <- as.integer(manifest$methods[[m]]$method_output_version)
    if (is.null(spec)) {
      cli::cli_abort(
        c(
          "The results contain method {.val {m}}, which {.pkg sepsisfactorsdb} cannot load.",
          "i" = "Supported methods: {.val {names(SUPPORTED_METHOD_OUTPUTS)}}. Request support by opening an issue."
        ),
        class = "sepsisfactorsdb_unsupported_method"
      )
    }
    if (!version %in% spec$versions) {
      cli::cli_abort(
        c(
          "Method {.val {m}} is at output version {version}; {.pkg sepsisfactorsdb} loads {cli::qty(length(spec$versions))}version{?s} {spec$versions}.",
          "i" = "Request support for the new output version by opening an issue."
        ),
        class = "sepsisfactorsdb_unsupported_method"
      )
    }
  }
  selected <- datasets %||% manifest$datasets
  absent <- setdiff(selected, manifest$datasets)
  if (length(absent)) {
    invalid_results("{cli::qty(length(absent))}Dataset{?s} {.val {absent}} not in the manifest of {.file {results_root}}.")
  }
  for (id in selected) validate_dataset_results(results_root, id, manifest)
  validate_cross_results(results_root)
  invisible(manifest)
}
