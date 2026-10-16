# parts of one unit must be adjacent, and parts must exist

    Code
      merge_parts(c(a, b, a), merge_dir = file.path(dir, "m"), units = c(1, 2, 1))
    Condition
      Error in `merge_parts()`:
      ! The parts of unit 1 are not adjacent in `part_paths`.

---

    Code
      merge_parts(file.path(dir, "none.sqlite"), merge_dir = file.path(dir, "m"))
    Condition
      Error in `merge_parts()`:
      ! Part file '<tmp>' does not exist.

# check_database refuses foreign key violations

    Code
      check_database(db)
    Condition
      Error in `check_database()`:
      ! 1 foreign key violation in '<tmp>'.
      i First: table "fit_params", row 1, refers to "fits".

