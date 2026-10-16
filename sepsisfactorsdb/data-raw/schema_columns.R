# Internal data: the column names of every table in inst/sql/schema.sql (schema_columns_data).
# Run this after any change to inst/sql/schema.sql; the test in test-schema.R fails while the saved
# lists differ from the schema.
pkgload::load_all(".", quiet = TRUE)
schema_columns_data <- read_schema_columns()
usethis::use_data(schema_columns_data, internal = TRUE, overwrite = TRUE)
