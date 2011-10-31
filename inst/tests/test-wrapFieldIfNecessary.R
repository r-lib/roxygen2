library(stringr)
library(testthat)

wrap_field_if_necessary <- function(field, value) {
   # if (lines lengths exceed wrap threshold) {
   # strwrap(sprintf('%s: %s', field, value), exdent=4, width = width)
   return(0)
}

leftPadNSpaces <- function(string, n) {
  str_pad(string, width = (nchar(string) + n), side = "left")
}

leftPadNSpaces("test", n = 2)

context("Wrapping DESCRIPTION fields only when necessary")
test_that("Left-side padding works properly for n > 0", {
    expect_equal(leftPadNSpaces("test", n = 2), "  test" )
    expect_equal(leftPadNSpaces("test", n = 4), "    test")
  }
)

test_that("Left-side padding works for the empty string", {
    expect_equal(leftPadNSpaces("", n = 4), "    ")
  }
)