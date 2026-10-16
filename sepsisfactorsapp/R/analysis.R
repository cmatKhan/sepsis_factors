# Analyses on loadings and scores that the pages compute themselves (everything else is
# read from the database).

# Least-squares projection of fit A's loadings onto fit B's basis on shared genes (the
# coefficients projectR computes): rows are B's components, columns of A. NULL when fewer
# than 3 genes are shared.
loadings_projection <- function(loadings_a, loadings_b) {
  shared <- intersect(rownames(loadings_a), rownames(loadings_b))
  if (length(shared) < 3) {
    return(NULL)
  }
  a <- loadings_a[shared, , drop = FALSE]
  b <- loadings_b[shared, , drop = FALSE]
  P <- solve(crossprod(b) + diag(1e-8, ncol(b)), crossprod(b, a))
  list(projection = P, n_genes = length(shared))
}

# One-to-one matching of the rows and columns of a similarity matrix on |similarity|,
# taking the largest remaining |value| first (greedy, so not always the optimal
# assignment). Returns a two-column integer matrix (`a`, `b`) of matched row and column
# indices, `min(nrow, ncol)` rows.
greedy_match_abs <- function(sim) {
  k <- min(dim(sim))
  a <- integer(k)
  b <- integer(k)
  work <- abs(sim)
  for (i in seq_len(k)) {
    at <- arrayInd(which.max(work), dim(work))
    a[i] <- at[1]
    b[i] <- at[2]
    work[at[1], ] <- -Inf
    work[, at[2]] <- -Inf
  }
  cbind(a = a, b = b)
}
