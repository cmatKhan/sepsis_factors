# Lint the files given on the command line with lintr (configured by the root .lintr).
# Used by the `lintr` hook in .pre-commit-config.yaml. With --warn-only the lints are printed
# but do not fail the hook.
args <- commandArgs(trailingOnly = TRUE)
warn_only <- "--warn-only" %in% args
files <- setdiff(args, "--warn-only")

lints <- lapply(files, lintr::lint)
n <- sum(lengths(lints))
for (l in lints) {
  if (length(l)) print(l)
}
if (n > 0 && !warn_only) {
  quit(status = 1)
}
