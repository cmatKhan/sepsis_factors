# Dummy raw data for a config, so that its paths exist.
touch_dataset_files <- function(cfg, data_root) {
  ds <- cfg$dataset
  for (key in intersect(c("expression_path", "sample_metadata_path", "feature_metadata_path"), names(ds))) {
    path <- file.path(data_root, ds[[key]])
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    file.create(path)
  }
}

test_that("the known datasets are built in, gains is not", {
  ids <- names(discover_datasets(tempfile()))
  expect_length(ids, 35)
  expect_contains(ids, c("EARLI", "ANEMONES_DAY1", "SHIP-TREND", "hfgp-500fg", "ROSE_DAY2"))
  expect_false("gains" %in% ids)
})

test_that("every built-in config validates against the methods", {
  data_root <- withr::local_tempdir()
  manifest <- method_manifest()
  for (path in discover_datasets(tempfile())) {
    cfg <- yaml::read_yaml(path)
    touch_dataset_files(cfg, data_root)
    meta <- read_dataset_metadata(path, manifest, data_root)
    expect_true(file.exists(meta$dataset$preprocessing_script), info = path)
    expect_equal(basename(meta$dataset$preprocessing_script), "preprocess.R")
  }
})

test_that("built-in preprocessing scripts hold no machine paths", {
  scripts <- Sys.glob(file.path(builtin_datasets_dir(), "*", "preprocess.R"))
  expect_length(scripts, 35)
  text <- unlist(lapply(scripts, readLines))
  expect_false(any(grepl("hf_sepsis_collection|~/projects|/scratch/", text)))
})

test_that("use_dataset scaffolds a dataset that is then discovered and validates", {
  project <- withr::local_tempdir()
  suppressMessages(use_dataset("MY_DATASET", project))
  expect_true(file.exists(file.path(project, "datasets", "MY_DATASET", "config.yml")))
  expect_true(file.exists(file.path(project, "datasets", "MY_DATASET", "preprocess.R")))
  found <- discover_datasets(file.path(project, "datasets"))
  expect_contains(names(found), c("MY_DATASET", "EARLI"))

  data_root <- withr::local_tempdir()
  cfg <- yaml::read_yaml(found[["MY_DATASET"]])
  touch_dataset_files(cfg, data_root)
  meta <- read_dataset_metadata(found[["MY_DATASET"]], method_manifest(), data_root)
  expect_equal(meta$dataset$id, "MY_DATASET")
  expect_equal(meta$dataset$expression_path, file.path(data_root, "MY_DATASET", "expression.parquet"))

  expect_error(suppressMessages(use_dataset("MY_DATASET", project)), "already exists")
})

test_that("a project dataset replaces the built-in one of the same id", {
  project <- withr::local_tempdir()
  suppressMessages(use_dataset("EARLI", project))
  found <- discover_datasets(file.path(project, "datasets"))
  expect_equal(unname(found[["EARLI"]]), normalizePath(file.path(project, "datasets", "EARLI", "config.yml")))
})

test_that("a dataset whose id differs from its folder is refused", {
  project <- withr::local_tempdir()
  dir.create(file.path(project, "datasets", "FOLDER"), recursive = TRUE)
  writeLines("dataset:\n  id: OTHER\n", file.path(project, "datasets", "FOLDER", "config.yml"))
  expect_snapshot(discover_datasets(file.path(project, "datasets")), error = TRUE, transform = function(x) {
    gsub("'[^']*datasets/FOLDER", "'<project>/datasets/FOLDER", x)
  })
})

test_that("an unknown method or boolean-token key in a config is refused", {
  data_root <- withr::local_tempdir()
  work <- withr::local_tempdir()
  file.copy(dirname(discover_datasets(tempfile())[["EARLI"]]), work, recursive = TRUE)
  path <- file.path(work, "EARLI", "config.yml")
  cfg <- yaml::read_yaml(path)
  touch_dataset_files(cfg, data_root)

  bad <- cfg
  bad$methods$umap <- list(k = 2)
  yaml::write_yaml(bad, path)
  expect_error(read_dataset_metadata(path, method_manifest(), data_root), "not a method")

  bad <- cfg
  bad$methods$nmf <- stats::setNames(list(1:2), "FALSE")
  yaml::write_yaml(bad, path)
  expect_error(read_dataset_metadata(path, method_manifest(), data_root), "boolean-token")
})

test_that("the data root comes from the option, then the environment, then pipeline.yml", {
  yml <- withr::local_tempfile(fileext = ".yml")
  writeLines("data_root: /from/yaml", yml)
  withr::local_envvar(SEPSIS_DATA_ROOT = "")
  withr::local_options(sepsisfactors.data_root = NULL)
  expect_equal(default_data_root(yml), "/from/yaml")
  withr::local_envvar(SEPSIS_DATA_ROOT = "/from/env")
  expect_equal(default_data_root(yml), "/from/env")
  withr::local_options(sepsisfactors.data_root = "/from/option")
  expect_equal(default_data_root(yml), "/from/option")
})

test_that("relative paths resolve against the data root, absolute ones stay", {
  ds <- list(
    expression_path = "D/e.parquet", sample_metadata_path = "/abs/s.parquet",
    feature_metadata_path = "~/f.parquet", preprocessing_script = "preprocess.R"
  )
  out <- resolve_data_paths(ds, "/root", "/cfg/D")
  expect_equal(out$expression_path, "/root/D/e.parquet")
  expect_equal(out$sample_metadata_path, "/abs/s.parquet")
  expect_equal(out$feature_metadata_path, "~/f.parquet")
  expect_equal(out$preprocessing_script, "/cfg/D/preprocess.R")
})
