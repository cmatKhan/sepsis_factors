# The methods. The set is closed: pca, spca, ica, nmf, cogaps and wgcna. Each
# method file defines
#   - `<name>_registry`, the fitting contract (fit function, parameter grid,
#     defaults);
#   - `<name>_ingest`, the output contract: the method's properties, what its
#     fits are extracted to, and the behaviour the workflow derives from them
#     (see `METHOD_SPEC_DEFAULTS`).
# `method_manifest()` collects and validates them. Adding a method is a
# developer task: a new registry, spec and output version here, and the same
# in sepsisfactorsdb.

#' Run one fit, recording failures and wall time
#'
#' Wraps a fit function's body (every method) so a failing fit returns its
#' error message instead of stopping the rest of its batch; the extraction
#' records it as failed.
#'
#' @param expr The fit's body, evaluated lazily.
#' @param base Fields known before fitting (e.g. rank, seed), returned with
#'   `error` on failure.
#' @return The fit's result list plus `elapsed` (seconds), or `base` plus
#'   `error` and `elapsed` when the fit errored.
#' @noRd
capture_fit <- function(expr, base = list()) {
  t0 <- proc.time()[["elapsed"]]
  out <- tryCatch(expr, error = function(e) c(base, list(error = conditionMessage(e))))
  out$elapsed <- proc.time()[["elapsed"]] - t0
  out
}

#' Attach a package that only works attached
#'
#' @param pkg Package name.
#' @return `NULL`, invisibly.
#' @noRd
attach_namespace <- function(pkg) {
  if (!paste0("package:", pkg) %in% search()) {
    suppressPackageStartupMessages(attachNamespace(pkg))
  }
  invisible(NULL)
}

#' The fields of a method's output contract
#'
#' Every `<name>_ingest` sets `method_output_version`, `family`
#' (`"seed_sweep"` or `"param_grid"`), `sign_ambiguous`, `has_loadings`,
#' `rank_param` (the grid column holding the requested rank, or `NA`),
#' `resource_params` (parameters that only set parallelism, left out of a
#' fit's identity) and `extract(result, params)`. The rest default to a
#' generic loadings-and-scores method:
#'
#' - `representative`: which fits are representative: `"min_mse_per_rank"`
#'   (the lowest-mse seed at each rank; bootstrap resamples excluded),
#'   `"max_rank"` (the fit at the largest rank), `"all_ok"` or `"selected"`
#'   (the dataset's selection table).
#' - `optimal`: the fit compared across datasets: `"min_mean_mse"` (the
#'   lowest-mse seed at the rank with the lowest mean mse), `"first"`,
#'   `"max_metric"` (the representative fit with the largest
#'   `optimal_metric`) or `"none"`.
#' - `pairs`: how fits of the method are compared: `"loadings"` (Hungarian
#'   matching of loadings) or `"modules"` (gene Jaccard of modules).
#' - `redundancy`: whether within-fit redundancy and marker genes are computed.
#' - `markers`: `"generic"` (strongest relative loading) or `"cogaps"`
#'   (CoGAPS::patternMarkers() on the raw result).
#' - `icasso`: whether ICASSO clusters the method's components.
#' - `projection`: `"loadings"` projects the loadings as they are;
#'   `"prcomp"` rebuilds a prcomp object (loadings, centre, sdev).
#' - `factor_index`: `"position"` (the column) or `"module_id"` (the number in
#'   a score column's `ME<id>` name).
#'
#' @noRd
METHOD_SPEC_DEFAULTS <- list(
  representative = "min_mse_per_rank",
  optimal = "min_mean_mse",
  optimal_metric = NA_character_,
  pairs = "loadings",
  redundancy = TRUE,
  markers = "generic",
  icasso = FALSE,
  projection = "loadings",
  factor_index = "position"
)

#' The methods, with their registries and output specs
#'
#' @return Named list (by method, in a fixed order) of
#'   `list(registry, ingest, network)`, the shape the target factories take.
#' @keywords internal
#' @export
method_manifest <- function() {
  raw <- list(
    pca = list(pca_registry, pca_ingest),
    spca = list(spca_registry, spca_ingest),
    ica = list(ica_registry, ica_ingest),
    nmf = list(nmf_registry, nmf_ingest),
    cogaps = list(cogaps_registry, cogaps_ingest),
    wgcna = list(wgcna_registry, wgcna_ingest)
  )
  manifest <- list()
  for (key in names(raw)) {
    registry <- validate_method_registry(raw[[key]][[1]], key)
    spec <- validate_method_spec(raw[[key]][[2]], key)
    manifest[[key]] <- list(registry = registry, ingest = spec, network = isTRUE(registry$network))
  }
  manifest
}

#' One method's fitting contract
#'
#' @param method Method name.
#' @return The `<name>_registry` list.
#' @keywords internal
#' @export
method_registry <- function(method) method_manifest()[[method]]$registry

#' One method's output spec
#'
#' @param method Method name.
#' @return The validated `<name>_ingest` list, defaults filled in.
#' @keywords internal
#' @export
method_spec <- function(method) method_manifest()[[method]]$ingest

#' Validate a fitting contract
#'
#' @param registry A `<name>_registry` list.
#' @param key The method name.
#' @return `registry`, invisibly.
#' @noRd
validate_method_registry <- function(registry, key) {
  required <- c("global_object", "fn", "build_grid")
  missing <- setdiff(required, names(registry))
  if (length(missing) > 0) {
    cli::cli_abort("{.code {key}_registry} is missing required field{?s}: {.field {missing}}")
  }
  if (!is.character(registry$global_object) || length(registry$global_object) != 1) {
    cli::cli_abort("{.code {key}_registry$global_object} must be a single string")
  }
  if (!is.function(registry$fn) || !is.function(registry$build_grid)) {
    cli::cli_abort("{.code {key}_registry}'s {.field fn} and {.field build_grid} must both be functions")
  }
  if (!is.null(registry$defaults) &&
    (!is.list(registry$defaults) || (length(registry$defaults) > 0 && is.null(names(registry$defaults))))) {
    cli::cli_abort("{.code {key}_registry$defaults} must be a named list")
  }
  invisible(registry)
}

#' Validate an output contract and fill in its defaults
#'
#' @param spec A `<name>_ingest` list.
#' @param key The method name.
#' @return `spec` with `METHOD_SPEC_DEFAULTS` filled in.
#' @noRd
validate_method_spec <- function(spec, key) {
  required <- c(
    "method_output_version", "family", "sign_ambiguous", "has_loadings", "rank_param",
    "resource_params", "extract"
  )
  missing <- setdiff(required, names(spec))
  if (length(missing) > 0) {
    cli::cli_abort("{.code {key}_ingest} is missing required field{?s}: {.field {missing}}")
  }
  spec <- utils::modifyList(METHOD_SPEC_DEFAULTS, spec)
  check <- function(field, allowed) {
    if (!spec[[field]] %in% allowed) {
      cli::cli_abort("{.code {key}_ingest${field}} must be one of {.val {allowed}}, not {.val {spec[[field]]}}")
    }
  }
  check("family", c("seed_sweep", "param_grid"))
  check("representative", c("min_mse_per_rank", "max_rank", "all_ok", "selected"))
  check("optimal", c("min_mean_mse", "first", "max_metric", "none"))
  check("pairs", c("loadings", "modules"))
  check("markers", c("generic", "cogaps"))
  check("projection", c("loadings", "prcomp"))
  check("factor_index", c("position", "module_id"))
  if (!is.function(spec$extract)) cli::cli_abort("{.code {key}_ingest$extract} must be a function")
  if (spec$optimal == "max_metric" && is.na(spec$optimal_metric)) {
    cli::cli_abort("{.code {key}_ingest$optimal_metric} is required when {.field optimal} is {.val max_metric}")
  }
  spec
}

#' The `methods` table of the results output
#'
#' @param manifest `method_manifest()` output.
#' @return Data frame (method, family, sign_ambiguous, has_loadings).
#' @keywords internal
#' @export
method_table <- function(manifest) {
  do.call(rbind, lapply(names(manifest), function(m) {
    s <- manifest[[m]]$ingest
    data.frame(
      method = m,
      family = s$family,
      sign_ambiguous = as.integer(isTRUE(s$sign_ambiguous)),
      has_loadings = as.integer(isTRUE(s$has_loadings))
    )
  }))
}
