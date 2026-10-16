# Missing and reduced data. Anything the app expects from the database that is not
# there is logged once to the console and shown as a notice; it never stops the app.
# Data that came from the reduced copy in the database, instead of the full artifact
# beside it, gets a "reduced data" notice that says what is reduced on that page.

# Log that something expected is missing (once per message and detail).
note_missing <- function(what, detail = NULL) {
  msg <- paste0("missing: ", what, if (!is.null(detail)) paste0(" (", detail, ")"))
  cli::cli_inform(
    c("!" = "[stability-app] {msg}"),
    .frequency = "once",
    .frequency_id = msg
  )
  invisible(NULL)
}

# An inline notice for a card whose data is missing.
missing_notice <- function(what, detail = NULL) {
  div(
    class = "alert alert-warning py-2 my-2", role = "alert",
    strong("Not available: "), what, if (!is.null(detail)) span(" ", detail)
  )
}

# Inside a render function: stop with a message (and log) when `x` has no data.
need_data <- function(x, what, hint = NULL) {
  if (is.null(x) || (is.data.frame(x) && nrow(x) == 0) || length(x) == 0) {
    note_missing(what, hint)
    shiny::validate(shiny::need(
      FALSE,
      paste0("Not available: ", what, if (!is.null(hint)) paste0(" (", hint, ")"))
    ))
  }
  invisible(x)
}

# Characters the pages show, written as code points so the source stays ASCII.
GLYPH_PLAY <- intToUtf8(0x25B6)
GLYPH_SQ <- intToUtf8(0xB2)
GLYPH_ARROW <- intToUtf8(0x2192)
GLYPH_DASH <- intToUtf8(0x2014)
GLYPH_NEWTAB <- intToUtf8(0x2197)

REDUCED_HINT <- paste(
  "The full sepsisfactors output, with the stability_artifacts/ folder beside the",
  "database, gives the complete data."
)

# The text of a reduced-data notice: what this page lacks, then where the rest is.
reduced_text <- function(what) {
  paste0("Reduced data: ", what, " ", REDUCED_HINT)
}

# The notice itself, for a page that is showing the database copy.
reduced_notice <- function(what) {
  div(
    class = "alert alert-info py-2 my-2", role = "note",
    strong("Reduced data: "), what, " ", REDUCED_HINT
  )
}

# Where a payload came from ("artifact" or "database"), kept as an attribute.
with_source <- function(x, source) {
  attr(x, "source") <- source
  x
}

from_database <- function(x) {
  identical(attr(x, "source"), "database")
}
