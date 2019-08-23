context("select_args")

test_that("errors on invalid input", {
  expect_error(select_args_text(sum, "-xlab:"), "Failed to parse")
  expect_error(select_args_text(sum, '"a"'), "numbers")

  f <- function(x, y, z) {}
  expect_error(select_args_text(f, "-x:z"), "numbers")
})

test_that("positive initial values starts from nothing", {
  f <- function(x, y, z) {}

  expect_equal(select_args_text(f, "x y"), c("x", "y"))
})

test_that("negative initial starts from everything", {
  f <- function(x, y, z) {}

  expect_equal(select_args_text(f, "-z"), c("x", "y"))
})

test_that("can alternative exclusion and inclusion", {
  f <- function(x, y, z) {}

  expect_equal(select_args_text(f, "-z z"), c("x", "y", "z"))
  expect_equal(select_args_text(f, "z -z"), character())
})
