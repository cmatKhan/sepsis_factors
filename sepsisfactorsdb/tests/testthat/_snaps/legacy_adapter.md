# copy mode copies and a file that cannot be placed is an error

    Code
      legacy_place(src, file.path(dir, "blocker", "x"), "symlink")
    Condition
      Error in `legacy_place()`:
      ! Could not symlink '<tmp>' to '<tmp>'.
      i Choose another `artifact_mode`; files are never copied unless `artifact_mode = "copy"`.

# a non-empty output, another schema version and an unknown dataset are refused

    Code
      legacy_to_results(leg$root, out, leg$config)
    Condition
      Error in `legacy_to_results()`:
      ! '<tmp>' is not empty.

---

    Code
      legacy_to_results(leg$root, file.path(withr::local_tempdir(), "r"), leg$config,
      datasets = "NOPE")
    Condition
      Error in `legacy_to_results()`:
      ! Dataset "NOPE" is not in '<tmp>'.

---

    Code
      legacy_to_results(leg$root, file.path(withr::local_tempdir(), "r"), leg$config)
    Condition
      Error in `legacy_to_results()`:
      ! '<tmp>' has schema version 2; the adapter reads version 3.

