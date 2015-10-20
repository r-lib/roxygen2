context("Usage")

# default usage ----------------------------------------------------------------

test_that("usage captured from formals", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    a <- function(a=1) {}")[[1]]
  expect_equal(get_tag(out, "usage")$values, rd("a(a = 1)"))
})

test_that("usage correct for modification functions", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    `foo<-` <- function(x, value) {}")[[1]]

  expect_equal(get_tag(out, "usage")$values, rd("foo(x) <- value"))
})

test_that("usage correct for functions with no arguments", {
  out <- roc_proc_text(rd_roclet(), "
      #' Function without parameters
      f <- function() 1")[[1]]

  expect_equal(get_tag(out, "usage")$values, rd("f()"))
})

test_that("default usage correct for infix functions", {
  out <- roc_proc_text(rd_roclet(), "
    #' Infix fun
    '%.%' <- function(a, b) 1")[[1]]

  expect_equal(get_tag(out, "usage")$values, rd("a \\%.\\% b"))
})

test_that("default usage correct for S3 methods", {
  out <- roc_proc_text(rd_roclet(), "
    #' Regular
    mean.foo <- function(x) 'foo'

    #' Infix
    '+.foo' <- function(x, b) 'foo'

    #' Modify
    '[<-.foo' <- function(x, value) 'foo'
  ")

  expect_equal(get_tag(out[[1]], "usage")$values, rd("\\method{mean}{foo}(x)"))
  expect_equal(get_tag(out[[2]], "usage")$values, rd("\\method{+}{foo}(x, b)"))
  expect_equal(get_tag(out[[3]], "usage")$values, rd("\\method{[}{foo}(x) <- value"))
})


test_that("default usage correct for S4 methods", {
  env <- pkg_env()
  setClass("foo", where = env)
  on.exit(removeClass("foo", where = env))
  out <- roc_proc_text(rd_roclet(), "
    #' Regular
    setMethod('sum', 'foo', function(x, ..., na.rm = FALSE) 'foo')

    #' Infix
    setMethod('+', 'foo', function(e1, e2) 'foo')

    #' Modify
    setMethod('[<-', 'foo', function(x, i, j, ..., value) 'foo')
  ")

  expect_equal(get_tag(out[[1]], "usage")$values,
    rd("\\S4method{sum}{foo}(x, ..., na.rm = FALSE)"))
  expect_equal(get_tag(out[[2]], "usage")$values,
    rd("\\S4method{+}{foo,ANY}(e1, e2)"))
  expect_equal(get_tag(out[[3]], "usage")$values,
    rd("\\S4method{[}{foo}(x, i, j, ...) <- value"))
})

test_that("default usage correct for S4 methods with different args to generic", {
  out <- roc_proc_text(rd_roclet(), "
    #' Generic
    #'
    #' @param x x
    #' @param ... arguments to methods
    setGeneric('testfun',function(x,...) standardGeneric('testfun'))

    #' Method
    #'
    #' @param add add
    setMethod('testfun','matrix',function(x, add = FALSE, ...){x - 1})
  ")

  expect_equal(get_tag(out[[2]], "usage")$value,
    rd("\\S4method{testfun}{matrix}(x, add = FALSE, ...)"))
})


test_that("non-syntactic S4 class names are escaped in usage", {
  out <- roc_proc_text(rd_roclet(), "
    setGeneric('rhs', function(x) standardGeneric('rhs'))

    #' Title.
    #'
    #' @inheritParams NULL
    setMethod('rhs', '<-', function(x) x[[3]])
  ")[[1]]

  expect_equal(get_tag(out, "usage")$value, rd('\\S4method{rhs}{`<-`}(x)'))
})


test_that("argument containing function is generates correct usage", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    a <- function(a= function(x) {1}) {}")[[1]]
  expect_equal(get_tag(out, "usage")$values, rd("a(a = function(x) {     1 })"))
})

test_that("backticks retained when needed", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    f <- function(`_x`) 1")[[1]]
  expect_equal(as.character(get_tag(out, "usage")$values), "f(`_x`)")
})


# @usage -----------------------------------------------------------------------

test_that("@usage overrides default", {
  out <- roc_proc_text(rd_roclet(), "
    #' @usage a(a=2)
    a <- function(a=1) {}")[[1]]
  expect_equal(get_tag(out, "usage")$values, rd("a(a=2)"))
})

test_that("@usage overrides default for @docType data", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    #'
    #' @name abc
    #' @docType data
    #' @usage data(abc)
    NULL")[[1]]

  expect_equal(get_tag(out, "usage")$values, rd("data(abc)"))
})

test_that("@usage NULL suppresses default usage", {
  out <- roc_proc_text(rd_roclet(), "
    #' @usage NULL
    a <- function(a=1) {}")[[1]]

  expect_equal(get_tag(out, "usage")$values, NULL)
})

test_that("quoted topics have usage statements", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    \"f\" <- function(a = 1, b = 2, c = a + b) {}")[[1]]

  expect_equal(get_tag(out, "usage")$values,
    rd("f(a = 1, b = 2, c = a + b)"))

  expect_equal(format(get_tag(out, "usage")),
    "\\usage{\nf(a = 1, b = 2, c = a + b)\n}\n"
  )

})

# Escaping --------------------------------------------------------------------

test_that("usage escaping preserved when combined", {
  out <- roc_proc_text(rd_roclet(), "
    #' Foo
    foo <- function(x = '%') x

    #' @rdname foo
    bar <- function(y = '%') y
  ")[[1]]

  expect_is(get_tag(out, "usage")$values, "rd")
})

test_that("default usage not double escaped", {
  out <- roc_proc_text(rd_roclet(), "
    #' Regular
    mean.foo <- function(x) 'foo'
  ")[[1]]

  expect_equal(format(get_tag(out, "usage")),
    "\\usage{\n\\method{mean}{foo}(x)\n}\n")
})

test_that("% and \\ are escaped in usage", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    a <- function(a='%\\\\') {}")[[1]]
  expect_equal(get_tag(out, "usage")$values, escape('a(a = "%\\\\")'))
  expect_equal(format(get_tag(out, "usage")),
    "\\usage{\na(a = \"\\%\\\\\\\\\")\n}\n")
})

test_that("% and \\ not escaped in manual usage", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    #' @usage %\\
    a <- function(a) {}
  ")[[1]]
  expect_equal(get_tag(out, "usage")$values, rd('%\\'))
  expect_equal(format(get_tag(out, "usage")), '\\usage{\n%\\\n}\n')
})

test_that("non-syntactic names are quoted", {

  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    'a b' <- function(x) x")[[1]]

  expect_equal(get_tag(out, "usage")$values, rd('"a b"(x)'))
})


test_that("Special vars removed in rc methods usage", {
  out <- roc_proc_text(rd_roclet(), "
    #' Class Blob
    ABCD <- setRefClass('ABC', methods = list(
      draw = function(x = 1) {
        \"2\"
        x
      })
    )
  ")[[1]]

  methods <- get_tag(out, "rcmethods")$values
  expect_equal(methods, list("draw(x = 1)" = "2"))
})

# Wrapping --------------------------------------------------------------------

test_that("long usages protected from incorrect breakage", {
  out <- roc_proc_text(rd_roclet(), "
    #' Function long usage
    f <- function(a = '                                    a',
                  b = '                                    b',
                  c = '                                    c',
                  d = '                                    d') 1")[[1]]

  usage <- format(get_tag(out, "usage"))
  expect_equal(str_count(usage, "\n"), 6)
})

test_that("argument vectors split on whitespace", {
  out <- roc_proc_text(rd_roclet(), "
    #' Function long usage
    f <- function(a = c('abcdef', 'abcdef', 'abcdef', 'abcdef', 'abcdef',
                  'abcdef', 'abcdef', 'abcdef'))  1")[[1]]

  usage <- get_tag(out, "usage")[[2]]
  expect_equal(str_count(usage, "\n"), 1)
})

test_that("\\method not split inappropriately", {
  out <- roc_proc_text(rd_roclet(), "
    #' Function long usage
    mean.reallyratherquitelongclassname <-
      function(reallyquitelongargument = 'reallyratherquitelongvalue') 1
  ")[[1]]
  usage <- format(get_tag(out, "usage"))
  expect_match(usage, "\\{mean\\}\\{reallyratherquitelongclassname\\}")
})

test_that("long usages protected from incorrect breakage", {
  out <- roc_proc_text(rd_roclet(), "
    #' Function long usage
    f <- function(a = '                                    a',
    b = '                                    b',
    c = '                                    c',
    d = '                                    d') 1")[[1]]

  usage <- format(get_tag(out, "usage"))
  expect_equal(str_count(usage, "\n"), 6)
})

test_that("breaking works after escapes (#265)", {

  out <- roc_proc_text(rd_roclet(), "
    #' Long args
    f <- function(
      xxxxxxxxxxxxxxxxxx1,
      xxxxxxxxxxxxxxxxxx2,
      xxxxxxxxxxxxxxxxxx3,
      x = \"\\\"'\",
      xxxxxxxxxxxxxxxxxx4,
      xxxxxxxxxxxxxxxxxx5,
      xxxxxxxxxxxxxxxxxx6,
      xxxxxxxxxxxxxxxxxx7
  ){}")[[1]]
  usage <- format(get_tag(out, "usage"))
  expect_equal(str_count(usage, "\n"), 5)
})
