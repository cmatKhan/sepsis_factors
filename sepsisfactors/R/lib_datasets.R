# Datasets. A dataset is a folder `<ID>/` holding `config.yml` and, usually, a
# `preprocess.R` that builds its matrix. The package ships the folders of the
# known datasets (`inst/datasets/`); a project's own `datasets/` folder adds
# datasets and overrides built-in ones of the same id. Raw data is not in the
# package: the configs give data paths relative to a data root.

#' The data root on this machine
#'
#' Where the downloaded sepsis collection lives. Taken from, in order, the
#' option `sepsisfactors.data_root`, the environment variable
#' `SEPSIS_DATA_ROOT`, and `data_root` in the project's `pipeline.yml`.
#'
#' @param pipeline_path Path to the project's `pipeline.yml`.
#' @return The data root, or `NULL` if none is set.
#' @export
default_data_root <- function(pipeline_path = "pipeline.yml") {
  opt <- getOption("sepsisfactors.data_root")
  if (!is.null(opt) && nzchar(opt)) {
    return(opt)
  }
  env <- Sys.getenv("SEPSIS_DATA_ROOT")
  if (nzchar(env)) {
    return(env)
  }
  if (file.exists(pipeline_path)) {
    return(yaml::read_yaml(pipeline_path)$data_root)
  }
  NULL
}

#' Resolve a dataset's relative data paths
#'
#' Prefixes `data_root` onto the relative `expression_path`,
#' `sample_metadata_path`, `feature_metadata_path` and `matrix_path` of a
#' config's `dataset:` block, and `config_dir` onto a relative
#' `preprocessing_script`; absolute and `~` paths are left as written.
#'
#' @param ds A config's `dataset:` block (named list).
#' @param data_root Data root directory, or `NULL` to leave paths unchanged.
#' @param config_dir The directory of the config file.
#' @return `ds` with its paths resolved.
#' @noRd
resolve_data_paths <- function(ds, data_root = default_data_root(), config_dir = NULL) {
  is_relative <- function(p) !is.null(p) && !startsWith(p, "/") && !startsWith(p, "~")
  if (!is.null(data_root)) {
    for (key in c("expression_path", "sample_metadata_path", "feature_metadata_path", "matrix_path")) {
      if (is_relative(ds[[key]])) ds[[key]] <- file.path(data_root, ds[[key]])
    }
  }
  if (!is.null(config_dir) && is_relative(ds$preprocessing_script)) {
    ds$preprocessing_script <- file.path(config_dir, ds$preprocessing_script)
  }
  ds
}

#' Read a dataset config with its paths resolved
#'
#' The one way configs should be read (`read_dataset_metadata()` adds
#' validation on top).
#'
#' @param path Path to a dataset's `config.yml`.
#' @param data_root Data root directory (`default_data_root()`).
#' @return The parsed config, with `resolve_data_paths()` applied to `dataset:`.
#' @noRd
read_dataset_yaml <- function(path, data_root = default_data_root()) {
  cfg <- yaml::read_yaml(path)
  cfg$dataset <- resolve_data_paths(cfg$dataset, data_root, dirname(normalizePath(path)))
  cfg
}

#' The folder of the datasets built into the package
#'
#' @return Path to `inst/datasets/` as installed.
#' @noRd
builtin_datasets_dir <- function() system.file("datasets", package = "sepsisfactors")

#' The config of every available dataset
#'
#' Built-in datasets, then the project's `datasets/` folder; a project dataset
#' with the same id replaces the built-in one. A dataset's id is the declared
#' `dataset.id` and must equal its folder name.
#'
#' @param datasets_dir The project's datasets folder (may not exist).
#' @return Named character vector: the `config.yml` path of each dataset,
#'   named by id.
#' @export
discover_datasets <- function(datasets_dir = "datasets") {
  found <- character(0)
  for (dir in c(builtin_datasets_dir(), datasets_dir)) {
    if (!nzchar(dir) || !dir.exists(dir)) next
    configs <- Sys.glob(file.path(dir, "*", "config.yml"))
    ids <- vapply(configs, function(f) {
      id <- yaml::read_yaml(f)$dataset$id %||% NA_character_
      if (!identical(id, basename(dirname(f)))) {
        cli::cli_abort("{.file {f}} declares dataset id {.val {id}}, but its folder is {.val {basename(dirname(f))}}.")
      }
      id
    }, character(1))
    found[ids] <- normalizePath(configs)
  }
  found
}

#' Find a dataset's config file
#'
#' @param pipeline The parsed pipeline config (`read_pipeline_config()`).
#' @param dataset_id Dataset id.
#' @return Path to the dataset's `config.yml`.
#' @noRd
dataset_config_path <- function(pipeline, dataset_id) {
  hit <- pipeline$dataset_configs[dataset_id]
  if (is.na(hit)) {
    cli::cli_abort(c(
      "No config for dataset {.val {dataset_id}}.",
      "i" = "Available: {.val {names(pipeline$dataset_configs)}}. Add a folder with {.fn use_dataset}."
    ))
  }
  unname(hit)
}

#' Start a new dataset
#'
#' Writes `<path>/datasets/<id>/config.yml` and `preprocess.R` from a
#' template, ready to edit. Name the dataset in `pipeline.yml`'s `datasets:`
#' list to include it in a run.
#'
#' @param id The new dataset's id; also its folder name.
#' @param path The project directory.
#' @return The dataset folder, invisibly.
#' @export
use_dataset <- function(id, path = ".") {
  dir <- file.path(path, "datasets", id)
  if (dir.exists(dir)) {
    cli::cli_abort("{.file {dir}} already exists; not overwriting it.")
  }
  dir.create(dir, recursive = TRUE)
  template <- system.file("templates", "dataset", package = "sepsisfactors")
  for (f in c("config.yml", "preprocess.R")) {
    txt <- gsub("__ID__", id, readLines(file.path(template, f)), fixed = TRUE)
    writeLines(txt, file.path(dir, f))
  }
  cli::cli_inform(c(
    "Wrote {.file {dir}}.",
    "i" = "Put the dataset's three parquet files under the data root, edit {.file config.yml}, and add {.val {id}} to {.field datasets:} in {.file pipeline.yml}."
  ))
  invisible(dir)
}

#' Download the sepsis collection from HuggingFace
#'
#' Downloads each dataset of a HuggingFace collection into `<dest>/<dataset>/`,
#' the layout the dataset configs expect (set `dest` as the data root). Needs
#' the `hf` command line tool (`pip install huggingface_hub`); datasets that
#' are already complete are skipped by `hf download`. ROSE comes from dbGaP and
#' is not in the collection.
#'
#' @param dest Directory to download into (the data root).
#' @param collection The collection's slug.
#' @param repos Dataset repositories to download instead of the whole
#'   collection (`"owner/name"`).
#' @return The dataset folders, invisibly.
#' @export
download_collection <- function(
  dest,
  collection = "cmatkhan/sepsis-transcriptomics-6a7a412da9a0c9fa16699858",
  repos = NULL
) {
  hf <- Sys.which("hf")
  if (!nzchar(hf)) {
    cli::cli_abort("The {.code hf} command was not found; install it with {.code pip install huggingface_hub}.")
  }
  if (is.null(repos)) {
    info <- system2(hf, c("collections", "info", collection, "--json"), stdout = TRUE)
    items <- jsonlite::fromJSON(paste(info, collapse = "\n"), simplifyVector = FALSE)$items
    repos <- vapply(Filter(function(i) identical(i$item_type, "dataset"), items), `[[`, "", "item_id")
  }
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(dest, basename(repos))
  for (i in seq_along(repos)) {
    cli::cli_inform("Downloading {.val {repos[i]}}")
    status <- system2(hf, c("download", repos[i], "--repo-type", "dataset", "--local-dir", out[i]))
    if (status != 0) cli::cli_abort("{.code hf download} failed for {.val {repos[i]}}.")
  }
  invisible(out)
}
