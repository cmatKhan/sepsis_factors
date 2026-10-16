# a gene that is not in the feature metadata is an error

    Code
      ingest_results(root, local_db())
    Message
      Loading 2 datasets into '<tmp>'.
    Condition
      Error in `gene_idx_of()`:
      ! 1 gene id in the loadings of "TOY1" is not in the dataset's feature metadata.
      i First: "NOT_A_GENE"

# a dataset without feature metadata cannot be numbered

    Code
      ingest_results(root, local_db())
    Message
      Loading 2 datasets into '<tmp>'.
    Condition
      Error in `steps[[i]]()`:
      ! Cannot number the genes of "TOY1".
      x The dataset needs a feature_metadata artifact with the column feature_id (its feature_id_col).
      i Reduced tables refer to genes by number, so the ids must be known.

# a projection without row and column names is refused

    Code
      encode_projection(list(projection = matrix(1:4, 2)), "p.rds")
    Condition
      Error in `encode_projection()`:
      ! 'p.rds' has no projection matrix with row and column names.

