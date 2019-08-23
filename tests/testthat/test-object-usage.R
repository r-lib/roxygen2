
test_that("usage captured from formals", {
  expect_equal(
    usage_from_call(f <- function() {}),
    "f()"
  )
  expect_equal(
    usage_from_call(f <- function(a = 1) {}),
    "f(a = 1)"
  )
})

test_that("argument containing function is generates correct usage", {
  expect_equal(
    usage_from_call(f <- function(a = function(x) 1) {}),
    "f(a = function(x) 1)"
  )
})

test_that("backticks retained when needed", {
  expect_equal(
    usage_from_call(f <- function(`_a`) {}),
    "f(`_a`)"
  )
})

test_that("default usage formats data correctly", {
  expect_equal(
    usage_from_call(hello <- 1),
    "hello"
  )
})

test_that("default usage formats replacement functions correctly", {
  expect_equal(
    usage_from_call(`f<-` <- function(x, value) {}),
    "f(x) <- value"
  )
  expect_equal(
    usage_from_call(`f<-` <- function(x, y, value) {}),
    "f(x, y) <- value"
  )
})

test_that("default usage formats infix functions correctly", {
  expect_equal(
    usage_from_call("%.%" <- function(a, b) {}),
    "a \\%.\\% b"
  )

  # even if it contains <-
  expect_equal(
    usage_from_call("%<-%" <- function(a, b) {}),
    "a \\%<-\\% b"
  )
})

test_that("default usage formats S3 methods correctly", {
  expect_equal(
    usage_from_call(mean.foo <- function(x) {}),
    "\\method{mean}{foo}(x)"
  )
  expect_equal(
    usage_from_call("+.foo" <- function(x, b) {}),
    "\\method{+}{foo}(x, b)"
  )
  expect_equal(
    usage_from_call("[<-.foo" <- function(x, value) {}),
    "\\method{[}{foo}(x) <- value"
  )
})

test_that("S4 classes have no default usage", {
  expect_equal(
    usage_from_call({
      setClass("Foo")
    }),
    character()
  )
})

test_that("default usage correct for S4 methods", {
  expect_equal(
    usage_from_call({
      setClass("Foo")
      setMethod("sum", "Foo", function(x, ..., na.rm = FALSE) {})
    }),
    "\\S4method{sum}{Foo}(x, ..., na.rm = FALSE)"
  )

  expect_equal(
    usage_from_call({
      setClass("Foo")
      setMethod("+", "Foo", function(e1, e2) "foo")
    }),
    "\\S4method{+}{Foo,ANY}(e1, e2)"
  )

  expect_equal(
    usage_from_call({
      setClass("Foo")
      setMethod("[<-", "Foo", function(x, i, j, ..., value) "foo")
    }),
    "\\S4method{[}{Foo}(x, i, j, ...) <- value"
  )
})

test_that("default usage correct for S4 methods with different args to generic", {
  expect_equal(
    usage_from_call({
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
    usage_from_call({
      setGeneric("rhs", function(x) standardGeneric("rhs"))
      setMethod("rhs", "<-", function(x) x[[3]])
    }),
    "\\S4method{rhs}{`<-`}(x)"
  )
})
