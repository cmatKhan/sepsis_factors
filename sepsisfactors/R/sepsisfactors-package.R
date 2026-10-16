#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats as.dist cutree kruskal.test median na.omit p.adjust prcomp quantile sd setNames
## usethis namespace: end
NULL

# stats::cor and stats::hclust are deliberately not imported: WGCNA evaluates
# its own cor() and hclust() from the calling frame, so an import here would
# shadow them and break blockwiseModules().

# Free variables of the fit functions: the matrix (`mat`, or `mat_nn` for the
# non-negative methods) and each column's subject are bound in the fit's
# environment by run_method_fits(). The rest are columns used in ggplot2
# aesthetics.
utils::globalVariables(c(
  "mat", "mat_nn", "subjects",
  ".data", "cell", "chrY_score", "inferred_sex", "nl_score", "present", "recorded_sex"
))
