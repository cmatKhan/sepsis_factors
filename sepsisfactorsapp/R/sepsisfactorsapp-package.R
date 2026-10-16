#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import shiny
#' @import bslib
#' @import ggplot2
#' @importFrom stats setNames
#' @importFrom utils head
#' @importFrom DT datatable DTOutput renderDT formatRound
## usethis namespace: end

## mockable bindings: start
## mockable bindings: end
NULL

# Column names used inside aes(), subset() and similar, which R CMD check reads as
# undefined globals.
utils::globalVariables(c(
  "Freq", "count", "dataset_id", "field", "g", "gene", "grp", "high", "index_of_sparseness",
  "iq", "kind", "kme_own", "label", "level", "loading", "logp", "low", "mean_diff", "method",
  "mid", "mse", "p95", "para", "pev", "power", "rank_key", "relative_r2", "score", "side", "sig",
  "stability", "statistic", "subject_id", "term", "text", "time", "time_all", "timepoint",
  "value", "weight", "x", "xist", "y", "chrY_score", "colour", "component", "dataset",
  "fraction", "inferred_sex", "nl_score", "pc1", "recorded", "share", "trait", "part",
  "category", "bin", "rho", "rank", "strip", "group", "subject", "module", "method_name"
))
