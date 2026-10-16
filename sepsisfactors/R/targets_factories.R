# Target factories (targets manual, "Target factories"): every target for one
# dataset, built from its config and the method registries, plus the
# cross-dataset targets. tar_target_raw() rather than tar_map() because each
# method's fit target needs its own packages and crew controller.

#' A dataset id made safe for target names
#'
#' @param dataset_id Dataset id.
#' @return The id with characters other than letters, digits and `_` replaced by `_`.
target_key <- function(dataset_id) gsub("[^A-Za-z0-9_]", "_", dataset_id)

#' Packages a target needs: this one, plus the given
#'
#' Target commands call this package's functions, so every target loads it;
#' a target that sets `packages` replaces targets' default instead of adding
#' to it.
#'
#' @param ... Further package names.
#' @return Character vector of package names.
#' @noRd
target_packages <- function(...) unique(c("sepsisfactors", ...))

#' Methods configured for a dataset
#'
#' A method runs when it appears under `methods:` (network methods under
#' `methods.network`).
#'
#' @param meta The dataset config.
#' @param manifest method_manifest() output.
#' @return Method names, in manifest order.
configured_methods <- function(meta, manifest) {
  flat <- setdiff(names(meta$methods), "network")
  nested <- names(meta$methods$network)
  intersect(names(manifest), c(flat, nested))
}

#' The files a dataset's matrix is built from
#'
#' The config, preprocessing script, prebuilt matrix and raw data files; the
#' `inputs_<dataset>` file target, so editing any of them rebuilds that dataset.
#'
#' @param config_path Path to the dataset config.
#' @param meta The resolved dataset config.
#' @return Character vector of existing paths (errors on a missing one).
dataset_input_files <- function(config_path, meta) {
  ds <- meta$dataset
  files <- c(config_path, ds$matrix_path, ds$preprocessing_script,
             ds$expression_path, ds$sample_metadata_path, ds$feature_metadata_path)
  missing <- files[!file.exists(files)]
  if (length(missing) > 0) {
    stop("Dataset '", ds$id, "' references files that don't exist on this machine: ",
         paste(missing, collapse = ", "),
         " -- check the data root (data_root in pipeline.yml, or SEPSIS_DATA_ROOT).")
  }
  unname(files)
}

#' Every target for one dataset
#'
#' Inputs, matrix, and per method the grid and fit targets (plus sPCA's refine
#' stage), then tar_dataset_ingest()'s targets.
#'
#' @param dataset_id Dataset id.
#' @param manifest method_manifest() output.
#' @param pipeline The parsed pipeline config.
#' @param data_root Data root directory.
#' @param variance_ids variance_datasets() output.
#' @param families The dataset families, parsed.
#' @return List of target objects.
tar_dataset_fits <- function(dataset_id, manifest, pipeline, data_root, variance_ids = character(0),
                             families = list()) {
  config_path <- dataset_config_path(pipeline, dataset_id)
  # Read once at definition time to decide which targets exist; the
  # meta_<key> target re-reads it at run time, so edits are tracked.
  meta <- read_dataset_metadata(config_path, manifest = manifest,
                                data_root = data_root)
  key <- target_key(dataset_id)
  name <- function(...) paste(c(..., key), collapse = "_")
  methods <- setdiff(configured_methods(meta, manifest), unlist(pipeline$skip_methods))

  unknown <- setdiff(methods, names(pipeline$controllers))
  if (length(unknown) > 0) {
    stop("No controller in pipeline.yml for method(s): ", paste(unknown, collapse = ", "))
  }

  targets <- list(
    targets::tar_target_raw(
      name("inputs"),
      as.call(c(quote(c), as.list(dataset_input_files(config_path, meta)))),
      format = "file",
      deployment = "main"
    ),
    targets::tar_target_raw(
      name("meta"),
      substitute(
        {
          INPUTS
          read_dataset_metadata(CONFIG, data_root = DATA_ROOT)
        },
        list(INPUTS = as.symbol(name("inputs")), CONFIG = config_path, DATA_ROOT = data_root)
      ),
      deployment = "main"
    ),
    targets::tar_target_raw(
      name("mat"),
      substitute(load_input_matrix(META), list(META = as.symbol(name("meta")))),
      resources = targets::tar_resources(crew = targets::tar_resources_crew(controller = "prep"))
    ),
    targets::tar_target_raw(
      name("sample_meta"),
      substitute(read_sample_metadata(META), list(META = as.symbol(name("meta")))),
      deployment = "main"
    ),
    targets::tar_target_raw(
      name("subjects"),
      substitute(fit_subjects(META, SM, MAT),
                 list(META = as.symbol(name("meta")), SM = as.symbol(name("sample_meta")),
                      MAT = as.symbol(name("mat")))),
      deployment = "main"
    )
  )

  needs_nonneg <- any(vapply(methods, function(m) isTRUE(manifest[[m]]$registry$needs_nonneg), logical(1)))
  if (needs_nonneg) {
    targets <- c(targets, list(targets::tar_target_raw(
      name("mat_nn"),
      substitute(shift_nonneg(MAT), list(MAT = as.symbol(name("mat")))),
      packages = target_packages("matrixStats"),
      deployment = "main"
    )))
  }

  for (method in methods) {
    registry <- manifest[[method]]$registry
    registry_sym <- substitute(method_registry(M), list(M = method))
    mat_sym <- as.symbol(if (isTRUE(registry$needs_nonneg)) name("mat_nn") else name("mat"))
    cpus <- pipeline$controllers[[method]]$cpus_per_task
    batch <- as.integer(pipeline$controllers[[method]]$batch_size %||% 1L)

    # One dynamic branch per batch of grid rows, on the method's controller.
    fit_target <- function(fit_name, grid_name) {
      targets::tar_target_raw(
        fit_name,
        substitute(
          run_method_fits(REGISTRY, GRID, MAT, SUBJECTS),
          list(REGISTRY = registry_sym, GRID = as.symbol(grid_name), MAT = mat_sym,
               SUBJECTS = as.symbol(name("subjects")))
        ),
        pattern = substitute(map(GRID), list(GRID = as.symbol(grid_name))),
        iteration = "list",
        packages = target_packages(registry$pkgs),
        resources = targets::tar_resources(crew = targets::tar_resources_crew(controller = method))
      )
    }

    targets <- c(targets, list(
      targets::tar_target_raw(
        name("grid", method),
        substitute(
          build_method_grid(REGISTRY, META, METHOD, CPUS, BATCH),
          list(REGISTRY = registry_sym, META = as.symbol(name("meta")), METHOD = method,
               CPUS = cpus, BATCH = batch)
        ),
        iteration = "group",
        deployment = "main"
      ),
      fit_target(name("fit", method), name("grid", method))
    ))

    # Optional data-driven second stage (registry `refine_grid`, e.g. spca's
    # Index-of-Sparseness search): its grid depends on the first stage's
    # fits and the dataset's PCA fit; `select_fits` then tabulates both.
    if (!is.null(registry$refine_grid)) {
      if (!"pca" %in% methods) {
        stop(dataset_id, ": method '", method, "' has a refine stage that needs the dataset's ",
             "PCA fit, but methods.pca isn't configured.")
      }
      targets <- c(targets, list(
        targets::tar_target_raw(
          name("grid", method, "refine"),
          substitute(
            build_refine_grid(REGISTRY, META, METHOD, CPUS, BATCH, FITS, PCA),
            list(REGISTRY = registry_sym, META = as.symbol(name("meta")), METHOD = method,
                 CPUS = cpus, BATCH = batch, FITS = as.symbol(name("fit", method)),
                 PCA = as.symbol(name("fit", "pca")))
          ),
          iteration = "group",
          deployment = "main"
        ),
        fit_target(name("fit", method, "refine"), name("grid", method, "refine")),
        targets::tar_target_raw(
          name("selection", method),
          substitute(
            select_method_fits(REGISTRY, META, METHOD, CPUS, FITS, REFINE, PCA),
            list(REGISTRY = registry_sym, META = as.symbol(name("meta")), METHOD = method, CPUS = cpus,
                 FITS = as.symbol(name("fit", method)),
                 REFINE = as.symbol(name("fit", method, "refine")),
                 PCA = as.symbol(name("fit", "pca")))
          ),
          deployment = "main"
        )
      ))
    }
  }
  c(targets, tar_dataset_ingest(dataset_id, methods, manifest, pipeline, name,
                                variance_parent(dataset_id, variance_ids, families)))
}

#' Ingestion targets for one dataset
#'
#' Per method: ingest, pairs, representatives, redundancy, ICASSO and
#' enrichment targets; WGCNA's ORA, per-fit kME and dataset diagnostics; per
#' dataset: metadata, matrix file, diagnostics, enrichment rows, drivers and
#' `db_<dataset>`, which writes the dataset's DB rows.
#'
#' @param dataset_id Dataset id.
#' @param methods The dataset's configured methods.
#' @param manifest discover_method_registries() output.
#' @param pipeline The parsed pipeline config.
#' @param name Function building this dataset's target names.
#' @param vparent variance_parent() of the dataset: itself, its family's
#'   parent, or `NULL` when no design applies.
#' @return List of target objects.
tar_dataset_ingest <- function(dataset_id, methods, manifest, pipeline, name, vparent = NULL) {
  results_root <- pipeline$results_root %||% "results"
  on_ingest <- targets::tar_resources(crew = targets::tar_resources_crew(controller = "ingest"))
  on_fgsea <- targets::tar_resources(crew = targets::tar_resources_crew(controller = "fgsea"))
  fgsea_cpus <- pipeline$controllers$fgsea$cpus_per_task %||% 1L
  sym <- function(...) as.symbol(name(...))
  spec_sym <- function(m) substitute(method_spec(M), list(M = m))
  own_design <- identical(vparent, dataset_id)
  parent_sym <- function(prefix) as.symbol(paste(prefix, target_key(vparent), sep = "_"))
  on_vp <- targets::tar_resources(crew = targets::tar_resources_crew(controller = "vp"))
  vp_cpus <- as.integer(pipeline$controllers$vp$cpus_per_task %||% 1L)
  targets <- list()
  parts <- list()
  fgsea_parts <- list()
  wgcna_parts <- list()

  for (method in methods) {
    registry <- manifest[[method]]$registry
    spec <- manifest[[method]]$ingest
    has_refine <- !is.null(registry$refine_grid)
    fits_expr <- if (has_refine) {
      substitute(c(flatten_fits(A), flatten_fits(B)),
                 list(A = sym("fit", method), B = sym("fit", method, "refine")))
    } else {
      substitute(flatten_fits(A), list(A = sym("fit", method)))
    }
    targets <- c(targets, list(
      targets::tar_target_raw(
        name("ingest", method),
        substitute(ingest_method_fits(FITS, SPEC, METHOD, ID, DB),
                   list(FITS = fits_expr, SPEC = spec_sym(method), METHOD = method, ID = dataset_id,
                        DB = results_root)),
        resources = on_ingest
      ),
      targets::tar_target_raw(
        name("pairs", method),
        substitute(compute_method_pairs(INGEST, SPEC, DB),
                   list(INGEST = sym("ingest", method), SPEC = spec_sym(method), DB = results_root)),
        packages = target_packages("clue", "mclust"),
        resources = on_ingest
      ),
      targets::tar_target_raw(
        name("representatives", method),
        substitute(representative_keys(INGEST, SPEC, SELECTION),
                   list(INGEST = sym("ingest", method), SPEC = spec_sym(method),
                        SELECTION = if (has_refine) sym("selection", method) else NULL)),
        deployment = "main"
      )
    ))
    part <- list(ingest = sym("ingest", method), pairs = sym("pairs", method),
                 representatives = sym("representatives", method),
                 selection = if (has_refine) sym("selection", method) else NULL)
    # Score-level variance partition of every fit's factors, with the
    # dataset's (or its parent's) design.
    if (!is.null(vparent)) {
      targets <- c(targets, list(targets::tar_target_raw(
        name("factor_vp", method),
        substitute(factor_variance_partition(INGEST, DESIGN, SUBSET, DB, CPUS),
                   list(INGEST = sym("ingest", method), DESIGN = parent_sym("design"),
                        SUBSET = !own_design, DB = results_root, CPUS = vp_cpus)),
        resources = on_vp
      )))
      part$factor_variance <- sym("factor_vp", method)
    }

    if (isTRUE(spec$has_loadings)) {
      if (isTRUE(spec$redundancy)) {
        targets <- c(targets, list(targets::tar_target_raw(
          name("redundancy", method),
          substitute(compute_method_redundancy(INGEST, REPS, DB),
                     list(INGEST = sym("ingest", method), REPS = sym("representatives", method), DB = results_root)),
          packages = target_packages(if (spec$markers == "cogaps") "CoGAPS"),
          resources = on_ingest
        )))
        part$redundancy <- sym("redundancy", method)
      }
      if (isTRUE(spec$icasso)) {
        targets <- c(targets, list(targets::tar_target_raw(
          name("icasso", method),
          substitute(compute_method_icasso(INGEST, DB), list(INGEST = sym("ingest", method), DB = results_root)),
          resources = on_ingest
        )))
        part$icasso <- sym("icasso", method)
      }
      # Enrichment: one branch per representative fit, trimmed to the
      # significant terms.
      targets <- c(targets, list(
        targets::tar_target_raw(
          name("fgsea_jobs", method),
          substitute(fgsea_jobs(INGEST, REPS, RED, DB),
                     list(INGEST = sym("ingest", method), REPS = sym("representatives", method),
                          RED = part$redundancy, DB = results_root)),
          deployment = "main"
        ),
        targets::tar_target_raw(
          name("fgsea", method),
          substitute(
            if (has_job(JOBS$fit_key)) {
              trim_enrichment_result(run_fgsea_job(ID, METHOD, JOBS$fit_key, JOBS$loadings_file, msigdb_pathways,
                                                   ENSEMBL, JOBS$cogaps_marker_genes[[1]], CPUS,
                                                   both_directions = SIGNED))
            },
            list(ID = dataset_id, METHOD = method, JOBS = sym("fgsea_jobs", method),
                 ENSEMBL = sym("ensembl_map"), CPUS = fgsea_cpus, SIGNED = isTRUE(spec$sign_ambiguous))),
          pattern = substitute(map(J), list(J = sym("fgsea_jobs", method))),
          iteration = "list",
          packages = target_packages("fgsea", "BiocParallel"),
          resources = on_fgsea
        )
      ))
      fgsea_parts <- c(fgsea_parts, sym("fgsea", method))
    } else {
      # WGCNA: module ORA per fit, kME per fit, dataset-level diagnostics.
      targets <- c(targets, list(
        targets::tar_target_raw(
          name("wgcna_ora_jobs"),
          substitute(wgcna_ora_jobs(INGEST), list(INGEST = sym("ingest", method))),
          deployment = "main"
        ),
        targets::tar_target_raw(
          name("wgcna_ora"),
          substitute(
            if (has_job(JOBS$fit_key)) {
              trim_enrichment_result(run_wgcna_ora_job(ID, JOBS$fit_key, JOBS$module_genes[[1]],
                                                       JOBS$universe_genes[[1]], msigdb_pathways, ENSEMBL, CPUS))
            },
            list(ID = dataset_id, JOBS = sym("wgcna_ora_jobs"), ENSEMBL = sym("ensembl_map"),
                 CPUS = fgsea_cpus)),
          pattern = substitute(map(J), list(J = sym("wgcna_ora_jobs"))),
          iteration = "list",
          packages = target_packages("fgsea", "BiocParallel"),
          resources = on_fgsea
        ),
        targets::tar_target_raw(
          name("wgcna_fit_jobs"),
          substitute(unique(INGEST$modules$fit_key) %||% NA_character_, list(INGEST = sym("ingest", method))),
          deployment = "main"
        ),
        targets::tar_target_raw(
          name("wgcna_fit_diag"),
          substitute(if (has_job(KEY)) wgcna_fit_diagnostics(KEY, INGEST, MAT, DB),
                     list(KEY = sym("wgcna_fit_jobs"), INGEST = sym("ingest", method), MAT = sym("mat"),
                          DB = results_root)),
          pattern = substitute(map(J), list(J = sym("wgcna_fit_jobs"))),
          iteration = "list",
          resources = on_ingest
        ),
        targets::tar_target_raw(
          name("wgcna_dataset_diag"),
          substitute(wgcna_dataset_diagnostics(MAT, META, SM),
                     list(MAT = sym("mat"), META = sym("meta"), SM = sym("sample_meta"))),
          packages = target_packages("WGCNA"),
          resources = on_ingest
        )
      ))
      wgcna_parts <- c(wgcna_parts, sym("wgcna_ora"))
      part$wgcna_fits <- sym("wgcna_fit_diag")
    }
    parts[[method]] <- as.call(c(quote(list), Filter(Negate(is.null), part)))
  }

  loadings_methods <- Filter(function(m) isTRUE(manifest[[m]]$ingest$has_loadings), methods)
  rep_list <- as.call(c(quote(list), lapply(setNames(nm = loadings_methods), function(m) sym("representatives", m))))
  ing_list <- as.call(c(quote(list), lapply(setNames(nm = loadings_methods), function(m) sym("ingest", m))))
  redundancy_methods <- Filter(function(m) isTRUE(manifest[[m]]$ingest$redundancy), loadings_methods)
  red_list <- as.call(c(quote(list), lapply(setNames(nm = redundancy_methods),
                                            function(m) sym("redundancy", m))))
  # Repeated-measures inputs of the DB write: the dataset's own variance
  # results, or (a per-timepoint subset) its parent's design and sample QC.
  variance_call <- NULL
  variance_targets <- list()
  if (!is.null(vparent)) {
    variance_call <- substitute(
      variance_inputs(ID, PARENT, DESIGN, COMP, colnames(MAT), SUMMARY, EXPLORE),
      list(ID = dataset_id, PARENT = vparent, DESIGN = parent_sym("design"), COMP = parent_sym("composition"),
           MAT = sym("mat"), SUMMARY = if (own_design) sym("vp_summary") else NULL,
           EXPLORE = if (own_design) sym("vp_explore") else NULL))
    if (own_design) {
      variance_targets <- list(targets::tar_target_raw(
        name("variance_files"),
        substitute(write_variance_artifacts(ID, DB, VP, DE, COMP, EXPLORE),
                   list(ID = dataset_id, DB = results_root, VP = sym("vp"), DE = sym("de_tables"),
                        COMP = sym("composition"), EXPLORE = sym("vp_explore"))),
        format = "file",
        resources = on_ingest
      ))
    }
  }
  c(targets, variance_targets, list(
    targets::tar_target_raw(
      name("factor_gene_files"),
      substitute(write_factor_gene_artifacts(ID, DB, ENRICH, REDS),
                 list(ID = dataset_id, DB = results_root, ENRICH = sym("enrichment"), REDS = red_list)),
      format = "file",
      deployment = "main"
    ),
    targets::tar_target_raw(
      name("metadata_files"),
      substitute(write_metadata_files(META, SM, DB), list(META = sym("meta"), SM = sym("sample_meta"), DB = results_root)),
      format = "file",
      deployment = "main"
    ),
    # A file target: projections onto this dataset read it.
    targets::tar_target_raw(
      name("matrix_file"),
      substitute(dataset_matrix_file(MAT, ID, DB), list(MAT = sym("mat"), ID = dataset_id, DB = results_root)),
      format = "file",
      deployment = "main"
    ),
    targets::tar_target_raw(
      name("matrix_diagnostics"),
      substitute(matrix_diagnostics_row(MAT), list(MAT = sym("mat"))),
      resources = on_ingest
    ),
    targets::tar_target_raw(
      name("ensembl_map"),
      substitute(build_ensembl_map(META, MAT), list(META = sym("meta"), MAT = sym("mat"))),
      deployment = "main"
    ),
    targets::tar_target_raw(
      name("enrichment"),
      # Each part is a pattern's list of branch values: flatten one level.
      substitute(enrichment_rows(unlist(F, recursive = FALSE), unlist(W, recursive = FALSE)),
                 list(F = as.call(c(quote(list), fgsea_parts)), W = as.call(c(quote(list), wgcna_parts)))),
      deployment = "main"
    ),
    targets::tar_target_raw(
      name("drivers"),
      substitute(pattern_driver_rows(INGESTS, REPS, MAT, SM, META$dataset$sample_id_col %||% "sample_id", ID, DB),
                 list(INGESTS = ing_list, REPS = rep_list, MAT = sym("mat"), SM = sym("sample_meta"),
                      META = sym("meta"), ID = dataset_id, DB = results_root)),
      packages = target_packages("projectR"),
      resources = on_ingest
    ),
    # The dataset's tables in the results output (a file target: the table
    # files). Artifacts were written by the targets above.
    targets::tar_target_raw(
      name("results"),
      substitute(
        write_dataset_results(ROOT, META, c(MF, MD, VF, FG), METHODS, method_table(method_manifest()),
                              ENRICH, MATDIAG, WGCNA, DRIVERS, VARIANCE),
        list(ROOT = results_root, META = sym("meta"), MF = sym("matrix_file"), MD = sym("metadata_files"),
             VF = if (own_design) sym("variance_files") else NULL, FG = sym("factor_gene_files"),
             METHODS = as.call(c(quote(list), parts)), ENRICH = sym("enrichment"),
             MATDIAG = sym("matrix_diagnostics"),
             WGCNA = if (any(!vapply(methods, function(m) isTRUE(manifest[[m]]$ingest$has_loadings), logical(1))))
               sym("wgcna_dataset_diag") else NULL,
             DRIVERS = sym("drivers"), VARIANCE = variance_call)),
      format = "file",
      deployment = "main"
    )
  ))
}

#' Cross-dataset targets
#'
#' Gathered Ensembl maps and matrix files, dataset families, per dataset its
#' projectR jobs and gene-space agreement, `results_cross_dataset`, which
#' writes them to the results output after every dataset's, and
#' `results_manifest`, the results output's manifest.
#'
#' @param pipeline The parsed pipeline config.
#' @param manifest method_manifest() output.
#' @param data_root Data root directory.
#' @return List of target objects.
tar_cross_dataset <- function(pipeline, manifest, data_root) {
  results_root <- pipeline$results_root %||% "results"
  ids <- unlist(pipeline$datasets)
  keys <- setNames(target_key(ids), ids)
  families <- yaml::read_yaml(pipeline$files$dataset_families)
  loadings_methods_of <- lapply(setNames(nm = ids), function(id) {
    meta <- read_dataset_metadata(dataset_config_path(pipeline, id), manifest = manifest, data_root = data_root)
    ms <- setdiff(configured_methods(meta, manifest), unlist(pipeline$skip_methods))
    Filter(function(m) isTRUE(manifest[[m]]$ingest$has_loadings), ms)
  })
  named_list <- function(prefix, which = ids) {
    as.call(c(quote(list), lapply(which, function(id) as.symbol(paste(prefix, keys[[id]], sep = "_")))
              |> setNames(which)))
  }
  parts_of <- function(id) {
    as.call(c(quote(list), lapply(setNames(nm = loadings_methods_of[[id]]), function(m) {
      substitute(list(ingest = I, representatives = R),
                 list(I = as.symbol(paste("ingest", m, keys[[id]], sep = "_")),
                      R = as.symbol(paste("representatives", m, keys[[id]], sep = "_"))))
    })))
  }

  targets <- list(
    targets::tar_target_raw("ensembl_maps", named_list("ensembl_map"), deployment = "main"),
    targets::tar_target_raw("matrix_files", substitute(unlist(X), list(X = named_list("matrix_file"))),
                            format = "file", deployment = "main"),
    targets::tar_target_raw("dataset_families_file", pipeline$files$dataset_families, format = "file",
                            deployment = "main"),
    targets::tar_target_raw("dataset_families", quote(yaml::read_yaml(dataset_families_file)),
                            deployment = "main")
  )
  on_projectr <- targets::tar_resources(crew = targets::tar_resources_crew(controller = "projectr"))
  projection_syms <- list(); score_syms <- list(); gene_space_syms <- list()
  for (id in ids) {
    key <- keys[[id]]
    nm <- function(prefix) paste(prefix, key, sep = "_")
    if (length(ids) > 1) {
      targets <- c(targets, list(
        targets::tar_target_raw(
          nm("projectr_jobs"),
          substitute(projectr_jobs(ID, PARTS, matrix_files, dataset_families, DB),
                     list(ID = id, PARTS = parts_of(id), DB = results_root)),
          iteration = "group",
          deployment = "main"
        ),
        targets::tar_target_raw(
          nm("projectr"),
          substitute(projection_rows(JOBS, ID, ensembl_maps, MATRIX_FILE, DB),
                     list(JOBS = as.symbol(nm("projectr_jobs")), ID = id,
                          MATRIX_FILE = as.symbol(nm("matrix_file")), DB = results_root)),
          pattern = substitute(map(J), list(J = as.symbol(nm("projectr_jobs")))),
          iteration = "list",
          packages = target_packages("projectR"),
          resources = on_projectr
        ),
        targets::tar_target_raw(
          nm("projection_scores"),
          substitute(projection_scores(JOBS, ID, ensembl_maps),
                     list(JOBS = as.symbol(nm("projectr_jobs")), ID = id)),
          resources = targets::tar_resources(crew = targets::tar_resources_crew(controller = "ingest"))
        )
      ))
      projection_syms <- c(projection_syms, as.symbol(nm("projectr")))
      score_syms <- c(score_syms, as.symbol(nm("projection_scores")))
    }
    kin <- Filter(function(b) b != id && same_family(families, id, b), ids)
    if (length(kin)) {
      targets <- c(targets, list(targets::tar_target_raw(
        nm("gene_space"),
        substitute(gene_space_rows(ID, PARTS, ensembl_maps, DB),
                   list(ID = id, DB = results_root,
                        PARTS = as.call(c(quote(list), lapply(setNames(nm = c(id, kin)), parts_of))))),
        packages = target_packages("projectR"),
        resources = targets::tar_resources(crew = targets::tar_resources_crew(controller = "ingest"))
      )))
      gene_space_syms <- c(gene_space_syms, as.symbol(nm("gene_space")))
    }
  }
  c(targets, list(
    targets::tar_target_raw(
      "results_cross_dataset",
      substitute(write_cross_results(ROOT, WRITTEN, unlist(P, recursive = FALSE), G, VE, SC),
                 list(ROOT = results_root, WRITTEN = named_list("results"),
                      P = as.call(c(quote(list), projection_syms)),
                      SC = as.call(c(quote(list), score_syms)),
                      G = as.call(c(quote(list), gene_space_syms)),
                      VE = if (length(variance_datasets(pipeline))) quote(variance_explore) else NULL)),
      format = "file",
      deployment = "main"
    ),
    targets::tar_target_raw(
      "results_manifest",
      substitute(write_results_manifest(ROOT, IDS, CROSS),
                 list(ROOT = results_root, IDS = ids, CROSS = quote(results_cross_dataset))),
      format = "file",
      deployment = "main"
    )
  ))
}
