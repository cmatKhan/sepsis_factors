test_that("float32 blobs round-trip, with NA as NaN", {
  x <- c(1.5, -2.25, NA, 0)
  expect_equal(decode_f32(encode_f32(x)), x)
  expect_equal(decode_f32(encode_f32(matrix(1:6, 2))), as.numeric(1:6))
})

test_that("int32 blobs round-trip", {
  m <- matrix(c(-1L, 2L, 3L, 4L), 2)
  expect_identical(decode_i32(encode_i32(m)), as.integer(m))
})

test_that("ids decode from JSON arrays, including the empty one", {
  expect_identical(decode_ids(encode_ids(c("a", "b c"))), c("a", "b c"))
  expect_identical(decode_ids("[]"), character(0))
})
