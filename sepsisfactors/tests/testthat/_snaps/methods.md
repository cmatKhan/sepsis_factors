# an invalid spec is rejected

    Code
      validate_method_spec(spec, "pca")
    Condition
      Error in `check()`:
      ! `pca_ingest$representative` must be one of "min_mse_per_rank", "max_rank", "all_ok", and "selected", not "best"

---

    Code
      validate_method_spec(spec, "pca")
    Condition
      Error in `validate_method_spec()`:
      ! `pca_ingest` is missing required field: family

---

    Code
      validate_method_spec(spec, "spca")
    Condition
      Error in `validate_method_spec()`:
      ! `spca_ingest$optimal_metric` is required when optimal is "max_metric"

