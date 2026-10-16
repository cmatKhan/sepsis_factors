test_that("artifact paths round trip", {
  expect_equal(artifact_rel("D", "a.rds"), "stability_artifacts/D/a.rds")
  expect_equal(artifacts_dir("/x/db.sqlite", "D"), "/x/stability_artifacts/D")
  db <- file.path(withr::local_tempdir(), "db.sqlite")
  rel <- artifact_rel("D", "a.rds")
  expect_equal(
    resolve_artifact(c(rel, NA, "", "/abs/p.rds"), db),
    c(file.path(normalizePath(dirname(db)), rel), NA, NA, "/abs/p.rds")
  )
})

test_that("place_artifacts links, copies and moves", {
  src <- withr::local_tempdir()
  dest <- withr::local_tempdir()
  writeLines("x", file.path(src, "a"))
  writeLines("y", file.path(src, "b"))
  writeLines("z", file.path(src, "c"))

  place_artifacts(file.path(src, "a"), file.path(dest, "sub", "a"), "link")
  expect_equal(readLines(file.path(dest, "sub", "a")), "x")
  expect_true(file.exists(file.path(src, "a")))

  place_artifacts(file.path(src, "b"), file.path(dest, "b"), "copy")
  expect_equal(readLines(file.path(dest, "b")), "y")
  expect_true(file.exists(file.path(src, "b")))

  place_artifacts(file.path(src, "c"), file.path(dest, "c"), "move")
  expect_equal(readLines(file.path(dest, "c")), "z")
  expect_false(file.exists(file.path(src, "c")))
})

test_that("symlink mode links to the source and never falls back to a copy", {
  src <- withr::local_tempdir()
  dest <- withr::local_tempdir()
  writeLines("x", file.path(src, "a"))
  place_artifacts(file.path(src, "a"), file.path(dest, "sub", "a"), "symlink")
  link <- file.path(dest, "sub", "a")
  expect_equal(unname(Sys.readlink(link)), normalizePath(file.path(src, "a")))
  expect_equal(readLines(link), "x")
  # a destination that cannot be linked is an error, not a copy
  blocker <- file.path(dest, "blocked")
  writeLines("y", blocker)
  expect_error(place_artifacts(file.path(src, "a"), file.path(blocker, "a"), "symlink"))
  expect_false(file.exists(file.path(blocker, "a")))
})

test_that("placing a file onto itself is a no-op", {
  dir <- withr::local_tempdir()
  writeLines("x", file.path(dir, "a"))
  place_artifacts(file.path(dir, "a"), file.path(dir, "a"), "move")
  expect_equal(readLines(file.path(dir, "a")), "x")
})

test_that("place_artifacts creates every destination directory", {
  src <- withr::local_tempdir()
  dest <- withr::local_tempdir()
  writeLines("x", file.path(src, "a"))
  writeLines("y", file.path(src, "b"))
  place_artifacts(
    file.path(src, c("a", "b")),
    file.path(dest, c("one", "two"), c("a", "b")),
    "copy"
  )
  expect_equal(readLines(file.path(dest, "one", "a")), "x")
  expect_equal(readLines(file.path(dest, "two", "b")), "y")
})
