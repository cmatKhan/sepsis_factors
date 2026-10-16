# Rebuilds tests/testthat/fixtures/stability.sqlite, the tiny schema-4 database the
# data-access tests read. Run from the sepsisfactorsapp directory:
#   Rscript dev/make_fixture.R
# It uses the sibling sepsisfactorsdb (example_results() and ingest_results()); the app
# itself never depends on that package. The tests read the committed file and never
# rebuild it. Only the database file is kept: the stability_artifacts/ folder the
# loader places beside it is left out, so the fixture is a database-only install.
db_dir <- file.path("..", "sepsisfactorsdb")
if (!dir.exists(db_dir)) {
  stop("The sibling sepsisfactorsdb directory is needed to build the fixture.")
}
pkgload::load_all(db_dir, quiet = TRUE)

work <- tempfile("fixture-")
results <- example_results(file.path(work, "results"))
built <- file.path(work, "stability.sqlite")
ingest_results(results, built)

# one plain file: no write-ahead log, no free pages
out <- file.path("tests", "testthat", "fixtures", "stability.sqlite")
unlink(out)
con <- DBI::dbConnect(RSQLite::SQLite(), built)
DBI::dbExecute(con, "PRAGMA wal_checkpoint(TRUNCATE)")
DBI::dbExecute(con, "PRAGMA journal_mode = DELETE")
DBI::dbExecute(con, sprintf("VACUUM INTO '%s'", out))
DBI::dbDisconnect(con)
message("Wrote ", out, " (", file.size(out), " bytes).")
