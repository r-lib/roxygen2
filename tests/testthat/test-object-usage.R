test_that("usage captured from formals", {
  expect_equal(
    call_to_usage(f <- function() {}),
    "f()"
  )
  expect_equal(
    call_to_usage(f <- function(a = 1) {}),
    "f(a = 1)"
  )
})

test_that("argument containing function is generates correct usage", {
  expect_equal(
    call_to_usage(f <- function(a = function(x) 1) {}),
    "f(a = function(x) 1)"
  )
})

test_that("backticks retained when needed", {
  expect_equal(
    call_to_usage(f <- function(`_a`) {}),
    "f(`_a`)"
  )
})

test_that("default usage formats data correctly", {
  expect_equal(
    call_to_usage(hello <- 1),
    "hello"
  )
})

test_that("default usage formats replacement functions correctly", {
  expect_equal(
    call_to_usage(`f<-` <- function(x, value) {}),
    "f(x) <- value"
  )
  expect_equal(
    call_to_usage(`f<-` <- function(x, y, value) {}),
    "f(x, y) <- value"
  )
})

test_that("default usage formats infix functions correctly", {
  expect_equal(
    call_to_usage("%.%" <- function(a, b) {}),
    "a \\%.\\% b"
  )

  # even if it contains <-
  expect_equal(
    call_to_usage("%<-%" <- function(a, b) {}),
    "a \\%<-\\% b"
  )
})

test_that("default usage formats S3 methods correctly", {
  expect_equal(
    call_to_usage(mean.foo <- function(x) {}),
    "\\method{mean}{foo}(x)"
  )
  expect_equal(
    call_to_usage(mean.function <- function(x) {}),
    "\\method{mean}{`function`}(x)"
  )
  expect_equal(
    call_to_usage("+.foo" <- function(x, b) {}),
    "\\method{+}{foo}(x, b)"
  )
  expect_equal(
    call_to_usage("[<-.foo" <- function(x, value) {}),
    "\\method{[}{foo}(x) <- value"
  )
})

test_that("S4 classes have no default usage", {
  expect_equal(
    call_to_usage({
      setClass("Foo")
    }),
    character()
  )
})

test_that("default usage correct for S4 methods", {
  expect_equal(
    call_to_usage({
      setClass("Foo")
      setMethod("sum", "Foo", function(x, ..., na.rm = FALSE) {})
    }),
    "\\S4method{sum}{Foo}(x, ..., na.rm = FALSE)"
  )

  expect_equal(
    call_to_usage({
      setClass("Foo")
      setMethod("+", "Foo", function(e1, e2) "foo")
    }),
    "\\S4method{+}{Foo,ANY}(e1, e2)"
  )

  expect_equal(
    call_to_usage({
      setClass("Foo")
      setMethod("[<-", "Foo", function(x, i, j, ..., value) "foo")
    }),
    "\\S4method{[}{Foo}(x, i, j, ...) <- value"
  )
})

test_that("default usage correct for S4 methods with different args to generic", {
  expect_equal(
    call_to_usage({
      setGeneric("testfun", function(x, ...) standardGeneric("testfun"))
      setMethod("testfun", "matrix", function(x, add = FALSE, ...) {
        x - 1
      })
    }),
    "\\S4method{testfun}{matrix}(x, add = FALSE, ...)"
  )
})

test_that("non-syntactic S4 class names are escaped in usage", {
  expect_equal(
    call_to_usage({
      setGeneric("rhs", function(x) standardGeneric("rhs"))
      setMethod("rhs", "<-", function(x) x[[3]])
    }),
    "\\S4method{rhs}{`<-`}(x)"
  )
})
