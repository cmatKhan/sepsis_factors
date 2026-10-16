# PCA (prcomp()). Deterministic, and its components are nested: the rank-k
# solution is the first k columns of any higher-rank one. So PCA is fit once
# per dataset at the largest configured `rank` (a list's maximum is used);
# `mse_by_rank` gives every truncation's reconstruction error, and choosing
# the first n components is left to readers. `rank` stands for prcomp()'s
# `rank.`. Fits the matrix `mat`.

#' Fit PCA at one rank
#'
#' @param rank Number of components kept.
#' @return `list(rank, seed = NA, mse, mse_by_rank, rotation, scores, sdev,
#'   center, elapsed)`; `rotation` is genes x components, `sdev` the full
#'   spectrum.
#' @noRd
run_pca_seed_sweep_job <- function(rank) {
  capture_fit(base = list(rank = rank, mse = NA_real_), {
    fit <- stats::prcomp(t(mat), rank. = rank, center = TRUE, scale. = FALSE)

    recon <- fit$x %*% t(fit$rotation)
    recon <- sweep(recon, 2, fit$center, "+")
    mse <- mean((t(mat) - recon)^2)

    # Reconstruction MSE of the rank-k truncation, k = 1..rank, from the full
    # spectrum: the centered data's squared singular values are
    # sdev^2 * (n - 1), and dropping components k+1.. leaves exactly their
    # sum as residual sum of squares (Eckart-Young). mse_by_rank[rank] == mse.
    n <- ncol(mat)
    ss <- fit$sdev^2 * (n - 1)
    mse_by_rank <- vapply(seq_len(rank), function(k) sum(ss[-seq_len(k)]), numeric(1)) / length(mat)

    list(
      rank = rank,
      seed = NA_integer_,
      mse = mse,
      mse_by_rank = mse_by_rank,
      rotation = fit$rotation,
      scores = fit$x,
      # sdev: prcomp()'s FULL per-component spectrum, regardless of rank.,
      # the only way to get per-component and cumulative proportion of
      # variance explained, never recoverable from the truncated rotation and
      # scores. center: needed with sdev and rotation to rebuild a
      # `prcomp`-classed object for projectR's PCA projection.
      sdev = fit$sdev,
      center = fit$center
    )
  })
}

pca_registry <- list(
  needs_nonneg = FALSE,
  global_object = "mat",
  fn = run_pca_seed_sweep_job,
  pkgs = character(0),
  defaults = list(rank = 20),
  # One fit at the largest configured rank (see header).
  build_grid = function(p) data.frame(rank = max(p$rank))
)

# The output contract: what a PCA fit looks like once extracted, and the
# behaviour the workflow derives from it. The fields are described in
# `method_spec()`.
pca_ingest <- list(
  method_output_version = 1L,
  family = "seed_sweep",
  sign_ambiguous = TRUE,
  has_loadings = TRUE,
  rank_param = "rank",
  resource_params = character(0),
  representative = "max_rank",
  optimal = "first",
  pairs = "loadings",
  redundancy = FALSE,
  projection = "prcomp",
  extract = function(result, params) {
    fit_record_from(
      rank = params$rank,
      mse = result$mse,
      loadings = result$rotation,
      scores = result$scores,
      diag = list(sdev = result$sdev, center = result$center, mse_by_rank = result$mse_by_rank)
    )
  }
)
