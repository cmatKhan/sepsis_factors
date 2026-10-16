# projections onto a dataset that is not loaded are refused

    Code
      load_cross_dataset(root, db)
    Condition
      Error in `load_cross_dataset()`:
      ! Projections target dataset "TOY2", which is not in the database.
      i Load every dataset the cross-dataset tables refer to first.

