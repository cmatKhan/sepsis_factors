# a gene that is not in the feature metadata is an error

    Code
      gene_idx_of(ctx, c("G1", "NOT_A_GENE"), "the loadings")
    Condition
      Error in `gene_idx_of()`:
      ! 1 gene id in the loadings of "D" is not in the dataset's feature metadata.
      i First: "NOT_A_GENE"

# a dataset without feature metadata cannot be numbered

    Code
      derive_genes(ctx)
    Condition
      Error in `derive_genes()`:
      ! Cannot number the genes of "D".
      x The dataset needs a feature_metadata artifact with the column gene_id (its feature_id_col).
      i Reduced tables refer to genes by number, so the ids must be known.

# a projection without row and column names is refused

    Code
      encode_projection(list(projection = matrix(1:4, 2)), "p.rds")
    Condition
      Error in `encode_projection()`:
      ! 'p.rds' has no projection matrix with row and column names.

