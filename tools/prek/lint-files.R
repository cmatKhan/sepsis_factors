# Lint the files given on the command line with lintr (configured by the root .lintr).
# Used by the `lintr` hook in .pre-commit-config.yaml. With --warn-only the lints are printed
# but do not fail the hook.
#
# lintr lints each file on its own, so object_usage_linter cannot see the functions and constants
# that a package defines in its other files and reports them as undefined. Each file's package
# (the nearest directory above it with a DESCRIPTION) is therefore loaded with pkgload first.
args <- commandArgs(trailingOnly = TRUE)
warn_only <- "--warn-only" %in% args
files <- setdiff(args, "--warn-only")

package_root <- function(file) {
  dir <- dirname(normalizePath(file))
  while (!file.exists(file.path(dir, "DESCRIPTION"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) {
      return(NA_character_)
    }
    dir <- parent
  }
  dir
}

roots <- unique(stats::na.omit(vapply(files, package_root, character(1))))
# Best effort: a package whose dependencies are not installed here (e.g. Bioconductor ones) is
# linted without being loaded, and object_usage_linter may then report its own functions as
# undefined.
for (root in roots) {
  tryCatch(
    pkgload::load_all(root, quiet = TRUE, helpers = FALSE, attach_testthat = FALSE),
    error = function(e) {
      message(
        "lintr hook: could not load ", basename(root), " (", conditionMessage(e), "); ",
        "object_usage lints for it may be false positives."
      )
    }
  )
}

lints <- lapply(files, lintr::lint)
n <- sum(lengths(lints))
for (l in lints) {
  if (length(l)) print(l)
}
if (n > 0 && !warn_only) {
  quit(status = 1)
}
