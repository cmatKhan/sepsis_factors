# A synthetic dataset small enough to run every analysis in the tests, and a
# project directory (pipeline.yml, dataset folders, _targets.R with local
# workers) around it.

synthetic_expression <- function(n_genes = 120, n_samples = 30, seed = 1) {
  set.seed(seed)
  genes <- sprintf("ENSG%011d", seq_len(n_genes))
  samples <- sprintf("S%02d", seq_len(n_samples))
  scores <- matrix(stats::rnorm(n_samples * 3), n_samples, 3)
  loadings <- matrix(0, n_genes, 3)
  for (k in 1:3) loadings[((k - 1) * 30 + 1):(k * 30), k] <- stats::rnorm(30, 2)
  mat <- loadings %*% t(scores) + matrix(stats::rnorm(n_genes * n_samples, sd = 0.5), n_genes) + 8
  dimnames(mat) <- list(genes, samples)
  mat
}

# The parquet trio of a dataset under `data_root/<id>/`.
write_synthetic_data <- function(data_root, id, seed = 1) {
  mat <- synthetic_expression(seed = seed)
  dir <- file.path(data_root, id)
  dir.create(dir, recursive = TRUE)
  long <- data.frame(
    feature_id = rep(rownames(mat), ncol(mat)),
    sample_id = rep(colnames(mat), each = nrow(mat)),
    value = as.vector(mat)
  )
  arrow::write_parquet(long, file.path(dir, "expression.parquet"))
  arrow::write_parquet(
    data.frame(sample_id = colnames(mat), group = rep(c("a", "b"), length.out = ncol(mat))),
    file.path(dir, "sample_metadata.parquet")
  )
  # Genes 1-5 are neutrophil and 6-10 lymphocyte markers, for the composition scores.
  symbols <- paste0("G", seq_len(nrow(mat)))
  symbols[1:10] <- c("FCGR3B", "CXCR1", "CXCR2", "CSF3R", "FPR1", "CD3D", "CD3E", "CD3G", "CD2", "CD5")
  arrow::write_parquet(
    data.frame(feature_id = rownames(mat), symbol = symbols),
    file.path(dir, "feature_metadata.parquet")
  )
  invisible(mat)
}

synthetic_config <- function(id, methods = c("pca", "spca", "ica", "nmf", "wgcna"), design = NULL) {
  blocks <- list(
    pca = "  pca:\n    rank: [3]",
    spca = paste(
      "  spca:", "    K: [2, 3]", "    K_scan: 2", "    para_scan: [0.3, 0.5]", "    para_step: 0.1",
      "    sparse: varnum", sep = "\n"
    ),
    ica = paste("  ica:", "    n.comp: [2, 3]", "    seed: [1, 2]", "    bootstrap: [false, true]", sep = "\n"),
    nmf = paste("  nmf:", "    k: [2, 3]", "    seed: [1, 2]", sep = "\n"),
    wgcna = paste(
      "  network:", "    wgcna:", "      power: [4, 6]", "      minModuleSize: 5", "      networkType: signed",
      sep = "\n"
    )
  )
  paste(
    c(
      "dataset:",
      paste0("  id: ", id),
      "  description: synthetic",
      paste0("  expression_path: ", id, "/expression.parquet"),
      paste0("  sample_metadata_path: ", id, "/sample_metadata.parquet"),
      paste0("  feature_metadata_path: ", id, "/feature_metadata.parquet"),
      "  sample_id_col: sample_id",
      "  feature_id_col: feature_id",
      "  value_col: value",
      "  ensembl_col: feature_id",
      "  symbol_col: symbol",
      if (!is.null(design)) design,
      "methods:",
      unlist(blocks[methods])
    ),
    collapse = "\n"
  )
}

# A project directory with the given synthetic datasets. Returns its paths.
local_project <- function(ids = c("SYN1", "SYN2"), methods = c("pca", "spca", "ica", "nmf", "wgcna"),
                          design = list(), env = parent.frame()) {
  root <- withr::local_tempdir("project-", .local_envir = env)
  data_root <- file.path(root, "data")
  project <- file.path(root, "project")
  dir.create(project)
  for (i in seq_along(ids)) {
    write_synthetic_data(data_root, ids[i], seed = i)
    dir.create(file.path(project, "datasets", ids[i]), recursive = TRUE)
    writeLines(synthetic_config(ids[i], methods, design[[ids[i]]]), file.path(project, "datasets", ids[i], "config.yml"))
  }
  template <- yaml::read_yaml(system.file("templates", "pipeline.yml", package = "sepsisfactors"))
  pipeline <- template
  pipeline$datasets <- as.list(ids)
  pipeline$data_root <- data_root
  pipeline$results_root <- "results"
  writeLines(yaml::as.yaml(pipeline), file.path(project, "pipeline.yml"))
  list(root = root, project = project, data_root = data_root, ids = ids)
}

fake_pathways <- function(n_genes = 120) {
  genes <- sprintf("ENSG%011d", seq_len(n_genes))
  sets <- list(set_a = genes[1:30], set_b = genes[31:60], set_c = genes[61:90], set_d = genes[91:120])
  stats::setNames(rep(list(sets), 6), c("HALLMARK", "GO:BP", "GO:MF", "KEGG", "REAC", "WP"))
}

# The package installed into a temporary library, once per session: targets
# loads it with library() in the process that builds each target.
installed_library <- local({
  lib <- NULL
  function() {
    if (is.null(lib)) {
      src <- normalizePath(testthat::test_path("..", ".."))
      if (!file.exists(file.path(src, "DESCRIPTION"))) {
        return(NULL)
      }
      lib <<- tempfile("lib-")
      dir.create(lib)
      status <- system2(
        file.path(R.home("bin"), "R"),
        c("CMD", "INSTALL", "--no-docs", "--no-test-load", paste0("--library=", lib), shQuote(src)),
        stdout = FALSE, stderr = FALSE
      )
      if (status != 0) {
        lib <<- NULL
      }
    }
    lib
  }
})

# Run the workflow in a project directory in this process (no workers).
make_project_targets <- function(project) {
  lib <- installed_library()
  testthat::skip_if(is.null(lib), "the package sources are not available to install")
  saveRDS(fake_pathways(), file.path(project, "pathways.rds"))
  writeLines(
    c(
      "library(targets)",
      "pipeline <- sepsisfactors::read_pipeline_config('pipeline.yml')",
      "targets::tar_option_set(packages = 'sepsisfactors', error = 'stop')",
      "sepsisfactors::pipeline_targets(pipeline, pathways = function() readRDS('pathways.rds'))"
    ),
    file.path(project, "_targets.R")
  )
  # A separate process, so that only the installed package is visible (not
  # devtools' view of the whole namespace).
  callr::r(
    function(project, lib) {
      .libPaths(c(lib, .libPaths()))
      setwd(project)
      targets::tar_make(callr_function = NULL, reporter = "silent")
    },
    args = list(project = project, lib = lib),
    libpath = c(lib, .libPaths())
  )
}
