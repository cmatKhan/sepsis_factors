# Check that the R files given on the command line parse.
for (f in commandArgs(trailingOnly = TRUE)) {
  parse(f, keep.source = FALSE)
}
