# a directory without a manifest is not a results output

    Code
      validate_results(dir)
    Condition
      Error in `read_manifest()`:
      ! No 'manifest.json' in '<tmp>'; is this a results output?

# another results-output version is refused

    Code
      validate_results(root)
    Condition
      Error in `validate_results()`:
      ! '<tmp>' has results-output version 2, but this package loads version 1.
      i Update sepsisfactors or sepsisfactorsdb so that the versions agree.

# an unknown method is refused with a pointer to the issue tracker

    Code
      validate_results(root)
    Condition
      Error in `validate_results()`:
      ! The results contain method "umap", which sepsisfactorsdb cannot load.
      i Supported methods: "pca", "spca", "ica", "nmf", "cogaps", and "wgcna". Request support by opening an issue.

# an unsupported method output version is refused

    Code
      validate_results(root)
    Condition
      Error in `validate_results()`:
      ! Method "pca" is at output version 2; sepsisfactorsdb loads version 1.
      i Request support for the new output version by opening an issue.

# a dataset missing from the manifest cannot be selected

    Code
      validate_results(root, "NOPE")
    Condition
      Error in `validate_results()`:
      ! Dataset "NOPE" not in the manifest of '<tmp>'.

# missing and unexpected columns are reported

    Code
      validate_results(root)
    Condition
      Error in `check_table_columns()`:
      ! Table "fit_params" in '<tmp>' does not match the results-output contract.
      x Missing column: text_value

---

    Code
      validate_results(root)
    Condition
      Error in `check_table_columns()`:
      ! Table "fit_params" in '<tmp>' does not match the results-output contract.
      x Unexpected column: extra

# a table holding another dataset's rows is refused

    Code
      validate_results(root)
    Condition
      Error in `validate_dataset_results()`:
      ! Table "samples" of "TOY1" holds rows for "TOY2".

# keys must refer to fits that exist

    Code
      validate_results(root)
    Condition
      Error in `check_keys()`:
      ! Table "fit_params" of "TOY1": 1 fit_key value not in "fits".
      i First: "pca_ffffffffffffffff"

# a missing artifact file is reported

    Code
      validate_results(root)
    Condition
      Error in `check_artifact_files()`:
      ! 1 artifact file referenced by fit_artifacts$path of "TOY1" is missing.
      i First: 'cogaps_000000000000362a_loadings.rds'

# an ok fit without the artifacts its method requires is refused

    Code
      validate_results(root)
    Condition
      Error in `check_method_outputs()`:
      ! 2 ok "nmf" fits in "TOY1" lack a "diag" artifact.
      i First: "nmf_0000000000002e88" and "nmf_0000000000003259"

# a methods row that disagrees with the method's output spec is refused

    Code
      validate_results(root)
    Condition
      Error in `check_method_outputs()`:
      ! Method "wgcna" in "TOY1": has_loadings is TRUE, version 1 of its output requires FALSE.

# a method that requires a table is refused without it

    Code
      validate_results(root)
    Condition
      Error in `check_method_outputs()`:
      ! Method "wgcna" in "TOY1" requires the table "gene_modules".

