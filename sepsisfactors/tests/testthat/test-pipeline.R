# A project with every built-in dataset's raw files present (empty).
local_full_project <- function(env = parent.frame()) {
  project <- withr::local_tempdir("project-", .local_envir = env)
  data_root <- withr::local_tempdir("data-", .local_envir = env)
  suppressMessages(use_pipeline(project))
  for (path in discover_datasets(tempfile())) {
    cfg <- yaml::read_yaml(path)
    for (key in intersect(c("expression_path", "sample_metadata_path", "feature_metadata_path"), names(cfg$dataset))) {
      f <- file.path(data_root, cfg$dataset[[key]])
      dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
      file.create(f)
    }
  }
  withr::local_envvar(SEPSIS_DATA_ROOT = data_root, .local_envir = env)
  project
}

test_that("use_pipeline writes the project files once", {
  dir <- withr::local_tempdir()
  suppressMessages(use_pipeline(dir))
  expect_setequal(list.files(dir), c("_targets.R", "pipeline.yml", "run_pipeline.sbatch"))
  expect_match(paste(readLines(file.path(dir, "_targets.R")), collapse = "\n"), "sepsisfactors::pipeline_targets")
  expect_error(suppressMessages(use_pipeline(dir)), "already exist")
})

test_that("the template pipeline.yml has a controller for every method and stage", {
  pipeline <- yaml::read_yaml(system.file("templates", "pipeline.yml", package = "sepsisfactors"))
  expect_contains(
    names(pipeline$controllers),
    c(names(method_manifest()), "prep", "ingest", "vp", "fgsea", "projectr")
  )
  expect_length(pipeline$datasets, 35)
  expect_true(all(unlist(pipeline$datasets) %in% names(discover_datasets(tempfile()))))
  expect_equal(pipeline$cluster$setup_lines, list())
})

test_that("the shipped files hold no machine-specific paths", {
  files <- c(
    system.file("templates", c("pipeline.yml", "run_pipeline.sbatch", "_targets.R"), package = "sepsisfactors"),
    Sys.glob(file.path(system.file("datasets", package = "sepsisfactors"), "*", "*"))
  )
  text <- unlist(lapply(files, readLines))
  expect_false(any(grepl("/scratch/|/ref/mblab|hf_sepsis_collection|chasem", text)))
})

test_that("a pipeline without a data root is refused", {
  project <- withr::local_tempdir()
  suppressMessages(use_pipeline(project))
  withr::local_envvar(SEPSIS_DATA_ROOT = "")
  withr::local_options(sepsisfactors.data_root = NULL)
  expect_error(read_pipeline_config(file.path(project, "pipeline.yml")), "No data root")
})

test_that("read_pipeline_config adds the datasets, data root, results root and shared files", {
  project <- local_full_project()
  pipeline <- read_pipeline_config(file.path(project, "pipeline.yml"))
  expect_length(pipeline$dataset_configs, 35)
  expect_equal(pipeline$data_root, Sys.getenv("SEPSIS_DATA_ROOT"))
  expect_equal(pipeline$results_root, "results")
  expect_true(all(file.exists(unlist(pipeline$files))))
})

test_that("a project's own families file replaces the built-in one", {
  project <- local_full_project()
  writeLines("A: [A]", file.path(project, "dataset_families.yml"))
  pipeline <- read_pipeline_config(file.path(project, "pipeline.yml"))
  expect_equal(pipeline$files$dataset_families, file.path(project, "dataset_families.yml"))
})

test_that("the whole pipeline's targets are defined for the built-in datasets", {
  project <- local_full_project()
  pipeline <- read_pipeline_config(file.path(project, "pipeline.yml"))
  targets <- pipeline_targets(pipeline, pathways = function() list())
  names <- vapply(unlist(targets, use.names = FALSE), function(t) t$settings$name, "")
  expect_gt(length(names), 800)
  expect_false(anyDuplicated(names) > 0)
  expect_contains(
    names,
    c("msigdb_pathways", "mat_EARLI", "fit_pca_EARLI", "fit_spca_refine_EARLI", "selection_spca_EARLI",
      "fgsea_nmf_EARLI", "wgcna_dataset_diag_EARLI", "results_EARLI", "results_hfgp_500fg",
      "projectr_EARLI", "gene_space_ANEMONES", "results_cross_dataset", "results_manifest",
      "vp_summary_EARLI", "variance_explore")
  )
  expect_false(any(grepl("^db_", names)))
})

test_that("a dataset with a refine stage needs the PCA fit", {
  project <- withr::local_tempdir()
  suppressMessages(use_pipeline(project))
  data_root <- withr::local_tempdir()
  suppressMessages(use_dataset("NOPCA", project))
  cfg_path <- file.path(project, "datasets", "NOPCA", "config.yml")
  cfg <- yaml::read_yaml(cfg_path)
  cfg$methods <- list(spca = list(K = 2))
  yaml::write_yaml(cfg, cfg_path)
  for (key in c("expression_path", "sample_metadata_path", "feature_metadata_path")) {
    f <- file.path(data_root, cfg$dataset[[key]])
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    file.create(f)
  }
  pipeline <- yaml::read_yaml(file.path(project, "pipeline.yml"))
  pipeline$datasets <- list("NOPCA")
  pipeline$data_root <- data_root
  yaml::write_yaml(pipeline, file.path(project, "pipeline.yml"))
  pipeline <- read_pipeline_config(file.path(project, "pipeline.yml"))
  expect_error(pipeline_targets(pipeline), "PCA fit")
})

test_that("the controller group has one controller per profile, backups included", {
  skip_if_not_installed("crew.cluster")
  pipeline <- yaml::read_yaml(system.file("templates", "pipeline.yml", package = "sepsisfactors"))
  group <- build_controller_group(pipeline, withr::local_tempdir())
  expect_contains(names(group$controllers), c("prep", "prep_backup", "pca", "spca_backup", "fgsea"))
})

test_that("worker script lines are the cluster setup lines, then a cd to the project", {
  expect_equal(
    worker_script_lines(list(setup_lines = list("module load R/4.6.1")), "/proj"),
    c("module load R/4.6.1", 'cd "/proj"')
  )
  expect_equal(worker_script_lines(list(setup_lines = NULL), "/proj"), 'cd "/proj"')
})
