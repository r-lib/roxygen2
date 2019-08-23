test_that("format has nice defaults for bare vectors", {
  expect_equal(
    call_to_format(x <- list(a = 1, b = 2)),
    "An object of class \\code{list} of length 2."
  )
})

test_that("format defaults for S3 objects", {
  expect_equal(
    call_to_format(x <- ordered(letters[1:5])),
    "An object of class \\code{ordered} (inherits from \\code{factor}) of length 5."
  )
})

test_that("format has nice defaults for matrices and arrays", {
  expect_equal(
    call_to_format(x <- diag(10)),
    "An object of class \\code{matrix} with 10 rows and 10 columns."
  )

  expect_equal(
    call_to_format(x <- array(1:27, dim = c(3, 3, 3))),
    "An object of class \\code{array} of dimension 3 x 3 x 3."
  )
})

test_that("format has nice defaults for data frames", {
  expect_equal(
    call_to_format(x <- data.frame(a = 1, b = 2)),
    "An object of class \\code{data.frame} with 1 rows and 2 columns."
  )
})
