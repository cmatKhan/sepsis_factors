# a dataset whose id differs from its folder is refused

    Code
      discover_datasets(file.path(project, "datasets"))
    Condition
      Error in `FUN()`:
      ! '<project>/datasets/FOLDER/config.yml' declares dataset id "OTHER", but its folder is "FOLDER".

