context("Rd - usage")
roc <- rd_roclet()

test_that("usage captured from formals", {
  out <- roc_proc_text(roc, "
    #' Title.
    a <- function(a=1) {}")[[1]]
  expect_equal(get_tag(out, "usage")$values, "a(a\u{A0}=\u{A0}1)")
})

test_that("usage correct for modification functions", {
  out <- roc_proc_text(roc, "
    #' Title.
    `foo<-` <- function(x, value) {}")[[1]]

  expect_equal(get_tag(out, "usage")$values, "foo(x) <- value")
})

test_that("usage correct for functions with no arguments", {
  out <- roc_proc_text(roc, "
      #' Function without parameters
      f <- function() 1")[[1]]

  expect_equal(get_tag(out, "usage")$values, "f()")
})

test_that("default usage correct for infix functions", {
  out <- roc_proc_text(roc, "
    #' Infix fun
    '%.%' <- function(a, b) 1")[[1]]

  expect_equal(get_tag(out, "usage")$values, "a \\%.\\% b")  
})

test_that("default usage correct for S3 methods", {
  out <- roc_proc_text(roc, "
    #' Regular
    mean.foo <- function(x) 'foo'

    #' Infix
    '+.foo' <- function(x, b) 'foo'

    #' Modify
    '[<-.foo' <- function(x, value) 'foo'
  ")
  
  expect_equal(get_tag(out[[1]], "usage")$values, "\\S3method{mean}{foo}(x)")
  expect_equal(get_tag(out[[2]], "usage")$values, "\\S3method{+}{foo}(x, b)")
  expect_equal(get_tag(out[[3]], "usage")$values, "\\S3method{[}{foo}(x) <- value")
})

test_that("default usage correct for S4 methods", {
  setClass("foo")
  on.exit(removeClass("foo"))
  out <- roc_proc_text(roc, "
    #' Regular
    setMethod('sum', 'foo', function(x, ..., na.rm = FALSE) 'foo')

    #' Infix
    setMethod('+', 'foo', function(e1, e2) 'foo')

    #' Modify
    setMethod('[<-', 'foo', function(x, i, j, ..., value) 'foo')
  ")
  
  expect_equal(get_tag(out[[1]], "usage")$values, 
    "\\S4method{sum}{foo}(x, ..., na.rm\u{A0}=\u{A0}FALSE)")
  expect_equal(get_tag(out[[2]], "usage")$values, 
    "\\S4method{+}{foo,ANY}(e1, e2)")
  expect_equal(get_tag(out[[3]], "usage")$values, 
    "\\S4method{[}{foo}(x, i, j, ...) <- value")
})


test_that("% and \\ are escaped in usage", {
  out <- roc_proc_text(roc, "
    #' Title.
    a <- function(a='%\\\\') {}")[[1]]
  expect_equal(get_tag(out, "usage")$values, 'a(a\u{A0}=\u{A0}"\\%\\\\\\\\")')
})

test_that("long usages protected from incorrect breakage", {
  out <- roc_proc_text(roc, "
      #' Function long usage
      f <- function(a = '                             a',
                    b = '                             b',
                    c = '                             c',
                    d = '                             ') 1")[[1]]

  usage <- format(get_tag(out, "usage"))
  expect_equal(str_count(usage, "\n"), 6)
})

test_that("@usage overrides default", {
  out <- roc_proc_text(roc, "
    #' @usage a(a=2)
    a <- function(a=1) {}")[[1]]
    expect_equal(get_tag(out, "usage")$values, "a(a=2)")
})

test_that("@usage overrides default for @docType data", {
  out <- roc_proc_text(roc, "
    #' Title.
    #'
    #' @name abc
    #' @docType data
    #' @usage data(abc)
    NULL")[[1]]

  expect_equal(get_tag(out, "usage")$values, "data(abc)")
})

test_that("quoted topics have usage statements", {
  out <- roc_proc_text(roc, "
    #' Title.
    \"f\" <- function(a = 1, b = 2, c = a + b) {}")[[1]]

  expect_equal(get_tag(out, "usage")$values,
    "f(a\u{A0}=\u{A0}1, b\u{A0}=\u{A0}2, c\u{A0}=\u{A0}a\u{A0}+\u{A0}b)")

  expect_equal(format(get_tag(out, "usage")),
    "\\usage{\n  f(a = 1, b = 2, c = a + b)\n}\n"
  )

})
