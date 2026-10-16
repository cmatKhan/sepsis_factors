test_that("map_ids returns ids and names the keys that have none", {
  ids <- c(a = 1L, b = 2L)
  expect_equal(map_ids(c("b", "a"), ids, "fit"), c(2L, 1L))
  expect_snapshot(map_ids(c("a", "x", "y"), ids, "fit"), error = TRUE)
  expect_error(map_ids("x", ids, "fit"), class = "sepsisfactorsdb_unmapped_key")
})

test_that("with_transaction rolls back when the code errors", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE t (x INTEGER)")
  expect_error(with_transaction(con, {
    DBI::dbExecute(con, "INSERT INTO t VALUES (1)")
    stop("boom")
  }), "boom")
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM t")$n, 0)
  with_transaction(con, DBI::dbExecute(con, "INSERT INTO t VALUES (1)"))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM t")$n, 1)
})

test_that("append_rows skips NULL and empty input and next_id counts on", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE t (id INTEGER PRIMARY KEY, x TEXT)")
  expect_equal(append_rows(con, "t", NULL), 0L)
  expect_equal(append_rows(con, "t", data.frame(id = integer(), x = character())), 0L)
  expect_equal(next_id(con, "t", "id"), 1L)
  append_rows(con, "t", data.frame(id = 4L, x = "a"))
  expect_equal(next_id(con, "t", "id"), 5L)
})
