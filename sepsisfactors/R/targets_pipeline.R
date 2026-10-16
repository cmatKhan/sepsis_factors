#' Every target of the workflow
#'
#' The MSigDB gene sets, each pipeline dataset's fit and extraction targets,
#' the cross-dataset targets, and the variance targets, ending in the targets
#' that write the results output. Use it in `_targets.R`, as [use_pipeline()]
#' sets up.
#'
#' @param pipeline The parsed pipeline config ([read_pipeline_config()]).
#' @param pathways A function returning the gene sets for enrichment: a named
#'   list by collection (`HALLMARK`, `GO:BP`, `GO:MF`, `KEGG`, `REAC`, `WP`) of
#'   named lists of gene sets in Ensembl gene ids. The default fetches them
#'   from MSigDB; give your own to work offline or with other gene sets.
#' @return A list of targets.
#' @export
pipeline_targets <- function(pipeline, pathways = fetch_msigdb_pathways) {
  manifest <- method_manifest()
  families <- yaml::read_yaml(pipeline$files$dataset_families)
  variance_ids <- variance_datasets(pipeline)
  list(
    # MSigDB gene sets for enrichment, fetched once on the main process.
    targets::tar_target_raw(
      "msigdb_pathways",
      as.call(list(pathways)),
      deployment = "main",
      packages = target_packages("msigdbr")
    ),
    lapply(
      pipeline$datasets,
      tar_dataset_fits,
      manifest = manifest,
      pipeline = pipeline,
      data_root = pipeline$data_root,
      variance_ids = variance_ids,
      families = families
    ),
    # Projections and gene-space agreement across datasets.
    tar_cross_dataset(pipeline, manifest, pipeline$data_root),
    # Variance partition and dream per dataset with a `design:` block;
    # independent of the fits.
    lapply(variance_ids, tar_dataset_variance, pipeline = pipeline),
    tar_variance_cross_dataset(pipeline, variance_ids)
  )
}

#' Set up a workflow project
#'
#' Writes `_targets.R`, `pipeline.yml` and `run_pipeline.sbatch` into a
#' directory. Edit `pipeline.yml` (the datasets to run, the data root, the
#' cluster settings), then run `targets::tar_make()` in that directory, or
#' `sbatch run_pipeline.sbatch` for a run that outlives your session.
#'
#' @param path The project directory (created if needed).
#' @return The project directory, invisibly.
#' @export
use_pipeline <- function(path = ".") {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  files <- c("_targets.R", "pipeline.yml", "run_pipeline.sbatch")
  existing <- files[file.exists(file.path(path, files))]
  if (length(existing)) {
    cli::cli_abort("{.file {existing}} already exist{?s} in {.file {path}}; not overwriting.")
  }
  for (f in files) {
    file.copy(system.file("templates", f, package = "sepsisfactors"), file.path(path, f))
  }
  cli::cli_inform(c(
    "Wrote {.file {files}} to {.file {path}}.",
    "i" = "Edit {.file pipeline.yml}, then run {.code targets::tar_make()}."
  ))
  invisible(path)
}
