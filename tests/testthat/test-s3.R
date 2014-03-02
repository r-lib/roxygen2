context("S3 methods/generics")

test_that("primitive generics detected", {
  expect_true(is_s3_generic("["))
  expect_true(is_s3_method("[.data.frame"))

  expect_true(is_s3_generic("mean"))
  expect_true(is_s3_method("mean.default"))

  expect_true(is_s3_generic("c"))
  expect_true(is_s3_method("c.Date"))
})

test_that("non-functions are not generics", {
  a <- TRUE
  b <- NULL
  c <- data.frame()

  expect_false(is_s3_generic("a"))
  expect_false(is_s3_generic("b"))
  expect_false(is_s3_generic("c"))
})

test_that("user defined generics & methods detected", {
  my_method <- function(x) UseMethod("mymethod")
  my_method.character <- function(x) x

  expect_true(is_s3_generic("my_method"))
  expect_true(is_s3_method("my_method.character"))
})


test_that("methods for group generics detected", {
  Ops.myclass <- function(x) x

  expect_false(is_s3_generic("Ops.myclass"))
  expect_true(is_s3_method("Ops.myclass"))
})


test_that("user defined generics detected even if use non-standard", {
  my_method <- function(x) {
    x <- 1
    if (x > 2) UseMethod("mymethod")
  }

  expect_true(is_s3_generic("my_method"))
})

test_that("user defined functions override primitives", {
  c <- function(x) x + 1
  c.test <- function(x) x + 3

  expect_false(is_s3_generic("c"))
  expect_false(is_s3_method("c"))
})

test_that("@method overrides auto-detection", {
  out <- parse_text("
    #' @export
    #' @method all.equal data.frame
    all.equal.data.frame <- function(...) 1
  ")$blocks[[1]]

  expect_equal(s3_method_info(out$object$value), c("all.equal", "data.frame"))
})
