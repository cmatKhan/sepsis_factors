# TEMPORARY copy of the factor matching of sepsisfactorsapp (R/similarity.R,
# itself a copy of sepsisfactors' R/analysis_similarity.R). The packages may not
# import each other, so the derive step carries the one function it needs.
# Removed with the derive step.

# Optimal 1-to-1 factor matching on |similarity| (Hungarian, clue::solve_LSAP).
# sim: factors_a x factors_b matrix. Returns a two-column integer matrix
# (a, b) of matched row and column indices.
hungarian_match_abs <- function(sim) {
  transposed <- FALSE
  if (nrow(sim) > ncol(sim)) {
    sim <- t(sim)
    transposed <- TRUE
  }
  a_sim <- abs(sim)
  cost <- max(a_sim) - a_sim
  assignment <- clue::solve_LSAP(cost)
  a <- seq_len(nrow(sim))
  b <- as.integer(assignment)
  if (transposed) cbind(a = b, b = a) else cbind(a = a, b = b)
}
