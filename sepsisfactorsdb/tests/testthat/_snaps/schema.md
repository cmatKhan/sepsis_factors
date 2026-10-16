# a database of another version is refused

    Code
      open_stability_db(db)
    Condition
      Error in `open_stability_db()`:
      ! '<tmp>' has schema version 2, but this package writes version 4.
      i Delete the '.sqlite' file (keep 'stability_artifacts/') and load again; there are no migrations.

