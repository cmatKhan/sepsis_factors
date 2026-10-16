# the Ensembl map is one-to-one: one feature per gene, the highest-mean one

    Code
      remap_to_ensembl(mat, c(p1 = "ENSG1", p2 = "ENSG1"))
    Condition
      Error:
      ! remap_to_ensembl(): the map sends several rows to one gene; build it with build_ensembl_map() from this dataset's matrix

