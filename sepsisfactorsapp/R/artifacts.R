# The full artifacts beside the database (the stability_artifacts/ folder written by
# sepsisfactorsdb). They are optional: every reader falls back to the reduced copy in
# the database. This is the only file that calls readRDS().

# The path of an artifact the database points at, or NA when there is none or its file
# is not there. Pointer paths are relative to the database's directory.
artifact_path <- function(db, path) {
  if (length(path) != 1 || is.na(path) || !nzchar(path)) {
    return(NA_character_)
  }
  file <- if (startsWith(path, "/")) path else file.path(db$dir, path)
  if (file.exists(file)) file else NA_character_
}

# The artifact's object, or NULL when it is absent or cannot be read.
read_artifact <- function(db, path) {
  file <- artifact_path(db, path)
  if (is.na(file)) {
    return(NULL)
  }
  tryCatch(
    readRDS(file),
    error = function(e) {
      note_missing("an artifact file", paste(basename(file), conditionMessage(e)))
      NULL
    }
  )
}
