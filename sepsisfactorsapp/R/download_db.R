DB_BASE_URL <- paste0(
  "https://huggingface.co/datasets/cmatkhan/sepsis_latent_factors_db_data/resolve/main/"
)
DB_FILE <- "db_no_rose.sqlite"
# Seconds allowed for the download (the file is several gigabytes).
DOWNLOAD_TIMEOUT <- 24 * 3600

#' Download the published database
#'
#' Fetches the SQLite database the app reads from the Hugging Face dataset
#' `cmatkhan/sepsis_latent_factors_db_data`. The file is several gigabytes. It is
#' downloaded under a temporary name next to its destination and renamed when it is
#' complete, so an interrupted download never leaves a partial file that looks whole. The
#' finished file must be an SQLite database whose schema version is the one this version of
#' the app reads (`PRAGMA user_version`); otherwise it is removed and the call fails.
#'
#' The published file holds the datasets that are not ROSE datasets: ROSE datasets are
#' not in it. It has no `stability_artifacts/` folder, so the app shows the reduced copies
#' stored in the database; see [run_app()].
#'
#' @param dest_dir Directory to save into (created if needed); the current working
#'   directory by default.
#' @param file Name of the file, on the server and on disk.
#' @param overwrite Replace the file if it already exists. By default an existing file is
#'   kept and its path returned without downloading.
#' @return The path of the database file, invisibly.
#' @examples
#' \dontrun{
#' download_db()
#' run_app("db_no_rose.sqlite")
#' }
#' @export
download_db <- function(dest_dir = ".", file = DB_FILE, overwrite = FALSE) {
  dest <- file.path(dest_dir, file)
  if (file.exists(dest) && !overwrite) {
    cli::cli_inform(c(
      "i" = "{.file {dest}} already exists; not downloading.",
      " " = "Use {.code overwrite = TRUE} to fetch it again."
    ))
    return(invisible(dest))
  }
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  partial <- tempfile(pattern = paste0(file, ".part-"), tmpdir = dest_dir)
  on.exit(unlink(partial), add = TRUE)
  url <- db_url(file)
  withr::local_options(timeout = max(getOption("timeout"), DOWNLOAD_TIMEOUT))
  cli::cli_inform("Downloading {.url {url}} to {.file {dest}}.")
  fetch_file(url, partial)
  check_downloaded_db(partial, url)
  if (file.exists(dest)) {
    unlink(dest)
  }
  if (!file.rename(partial, dest)) {
    cli::cli_abort("Could not move the downloaded file to {.file {dest}}.")
  }
  cli::cli_inform(c(
    "v" = "Saved {.file {dest}}.",
    "i" = "Start the app with {.fn run_app} and this path."
  ))
  invisible(dest)
}

# The download address of a published file.
db_url <- function(file) {
  paste0(DB_BASE_URL, file)
}

# Download `url` to `dest` in binary mode; a failure is an error.
fetch_file <- function(url, dest) {
  status <- tryCatch(
    utils::download.file(url, dest, mode = "wb", quiet = FALSE),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )
  if (!identical(status, 0L)) {
    cli::cli_abort(c(
      "Could not download {.url {url}}.",
      "x" = "{as.character(status)}"
    ))
  }
  invisible(dest)
}

# Check that `path` is an SQLite database at the schema version the app reads; if not,
# remove it and fail.
check_downloaded_db <- function(path, url = path) {
  problem <- db_file_problem(path)
  if (!is.null(problem)) {
    unlink(path)
    cli::cli_abort(c("The file downloaded from {.url {url}} cannot be used.", "x" = problem))
  }
  invisible(path)
}

# Why a file is not a usable database (NULL when it is).
db_file_problem <- function(path) {
  magic <- c(charToRaw("SQLite format 3"), as.raw(0))
  header <- readBin(path, "raw", n = 16)
  if (!identical(header, magic)) {
    return("It is not an SQLite database.")
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con))
  version <- as.integer(DBI::dbGetQuery(con, "PRAGMA user_version")[[1]])
  if (!identical(version, EXPECTED_SCHEMA_VERSION)) {
    return(sprintf(
      "It has schema version %d, but this app reads version %d.", version, EXPECTED_SCHEMA_VERSION
    ))
  }
  NULL
}
