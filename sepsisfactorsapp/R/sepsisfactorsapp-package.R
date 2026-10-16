#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import shiny
#' @import bslib
#' @import ggplot2
#' @importFrom stats aggregate cor cutree dist hclust median na.omit p.adjust reshape setNames
#' @importFrom utils head
#' @importFrom graphics layout par rect text
#' @importFrom DT datatable DTOutput renderDT formatPercentage formatRound formatStyle
## usethis namespace: end

## mockable bindings: start
## mockable bindings: end
NULL

# Column names used inside aes(), subset() and similar, which R CMD check reads
# as undefined globals (the copied code uses them unqualified).
utils::globalVariables(c(
  "Freq", "bin", "category", "chrY_score", "colour", "component", "count",
  "dataset", "dataset_id", "field", "fraction", "g", "gene", "group", "grp",
  "high", "index_of_sparseness", "inferred_sex", "iq", "kind", "kme_own",
  "label", "level", "loading", "logp", "low", "m", "mean_diff", "method",
  "mid", "module", "mse", "nl_score", "p95", "para", "part", "pc1", "power",
  "rank_key", "recorded", "relative_r2", "rho", "score", "share", "side",
  "sig", "stability", "statistic", "subject", "subject_id", "term", "time",
  "time_all", "timepoint", "trait", "value", "weight", "x", "xist", "y"
))
