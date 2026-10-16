test_that("the reduced-data text says what is reduced and where the rest is", {
  txt <- reduced_text("Loadings are limited to the top 50 genes.")
  expect_match(txt, "^Reduced data: Loadings are limited")
  expect_match(txt, "stability_artifacts/")
  expect_match(txt, "sepsisfactors output")
  html <- as.character(reduced_notice("Only significant genes."))
  expect_match(html, "Only significant genes.", fixed = TRUE)
  expect_match(html, "stability_artifacts/", fixed = TRUE)
})

test_that("a payload remembers where it came from", {
  x <- with_source(1:3, "database")
  expect_true(from_database(x))
  expect_false(from_database(with_source(1:3, "artifact")))
  expect_false(from_database(NULL))
})

test_that("missing data is logged once and need_data() stops quietly", {
  what <- paste("thing", runif(1))
  expect_message(note_missing(what), "missing")
  expect_no_message(note_missing(what))
  expect_error(
    suppressMessages(need_data(data.frame(), "a table")),
    class = "shiny.silent.error"
  )
  expect_error(
    suppressMessages(need_data(NULL, "a table")),
    class = "shiny.silent.error"
  )
  expect_equal(need_data(data.frame(a = 1), "a table"), data.frame(a = 1))
})
