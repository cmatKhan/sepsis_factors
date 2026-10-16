# Style the files given on the command line with styler (tidyverse style).
# Used by the `style-files` hook in .pre-commit-config.yaml. Files are rewritten in place; prek then
# reports the hook as failed so the changes can be reviewed and staged.
files <- commandArgs(trailingOnly = TRUE)
res <- styler::style_file(files, style = styler::tidyverse_style)
if (any(res$changed)) {
  cat("styled:", paste(res$file[res$changed], collapse = ", "), "\n")
}
