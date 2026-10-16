test_that("an artifact path is relative to the database's directory", {
  db <- fixture_copy()
  expect_true(is.na(artifact_path(db, NA_character_)))
  expect_true(is.na(artifact_path(db, "")))
  expect_true(is.na(artifact_path(db, "stability_artifacts/none.rds")))
  write_artifact(db, "stability_artifacts/a.rds", 1)
  expect_equal(
    artifact_path(db, "stability_artifacts/a.rds"),
    file.path(db$dir, "stability_artifacts/a.rds")
  )
  absolute <- file.path(db$dir, "stability_artifacts/a.rds")
  expect_equal(artifact_path(db, absolute), absolute)
})

test_that("read_artifact() returns the object, NULL when absent, NULL when unreadable", {
  db <- fixture_copy()
  expect_null(read_artifact(db, "stability_artifacts/none.rds"))
  write_artifact(db, "stability_artifacts/a.rds", list(x = 1))
  expect_equal(read_artifact(db, "stability_artifacts/a.rds"), list(x = 1))
  writeLines("not an rds", file.path(db$dir, "stability_artifacts/bad.rds"))
  expect_null(suppressMessages(read_artifact(db, "stability_artifacts/bad.rds")))
})
