# Decoders for the blobs of the derived tables. Blobs hold values without a header,
# column-major and little-endian, compressed with gzip: float32 for real numbers
# (NaN is NA) and int32 for integers. Row and column names are JSON arrays of strings.
# These are copies of the writer's decoders: the packages do not depend on each other.

# A float32 blob as a numeric vector.
decode_f32 <- function(blob) {
  raw <- memDecompress(blob, "gzip", asChar = FALSE)
  out <- readBin(raw, "numeric", n = length(raw) %/% 4L, size = 4L, endian = "little")
  out[is.nan(out)] <- NA_real_
  out
}

# An int32 blob as an integer vector.
decode_i32 <- function(blob) {
  raw <- memDecompress(blob, "gzip", asChar = FALSE)
  readBin(raw, "integer", n = length(raw) %/% 4L, size = 4L, endian = "little")
}

# A JSON array of strings as a character vector.
decode_ids <- function(json) {
  as.character(jsonlite::fromJSON(json))
}
