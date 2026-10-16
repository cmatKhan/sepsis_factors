# Builds the feature x sample matrix of the dataset __ID__.
#
# The workflow runs this script with these variables defined: expression_path,
# sample_metadata_path, feature_metadata_path, sample_id_col, feature_id_col,
# value_col, ensembl_col and tmp_dir. It must save the matrix (feature ids as
# row names, sample ids as column names) with
# saveRDS(mat, file.path(tmp_dir, "matrix.rds")).
#
# Delete this file, and `preprocessing_script` in config.yml, to use a plain
# pivot of the expression file instead.

raw <- list(
  ex = arrow::read_parquet(expression_path),
  meta = arrow::read_parquet(sample_metadata_path),
  feature = arrow::read_parquet(feature_metadata_path)
)

mat <- tidyr::pivot_wider(
  raw$ex[, c(feature_id_col, sample_id_col, value_col)],
  id_cols = dplyr::all_of(feature_id_col),
  names_from = dplyr::all_of(sample_id_col),
  values_from = dplyr::all_of(value_col)
)
rn <- mat[[feature_id_col]]
mat <- as.matrix(mat[, setdiff(names(mat), feature_id_col)])
rownames(mat) <- rn

saveRDS(mat, file.path(tmp_dir, "matrix.rds"))
