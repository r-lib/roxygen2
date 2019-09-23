test_that("@usage overrides default", {
  out <- roc_proc_text(rd_roclet(), "
    #' A
    #' @usage a(a=2)
    a <- function(a=1) {}")[[1]]
  expect_equal(out$get_value("usage"), rd("a(a=2)"))
})

test_that("@usage overrides default for @docType data", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    #'
    #' @name abc
    #' @docType data
    #' @usage data(abc)
    NULL")[[1]]

  expect_equal(out$get_value("usage"), rd("data(abc)"))
})

test_that("@usage NULL suppresses default usage", {
  out <- roc_proc_text(rd_roclet(), "
    #' A
    #' @usage NULL
    a <- function(a=1) {}")[[1]]

  expect_equal(out$get_value("usage"), NULL)
})

test_that("quoted topics have usage statements", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    \"f\" <- function(a = 1, b = 2, c = a + b) {}")[[1]]

  expect_equal(out$get_value("usage"), rd("f(a = 1, b = 2, c = a + b)"))
  expect_equal(out$get_rd("usage"), "\\usage{\nf(a = 1, b = 2, c = a + b)\n}")
})

# Escaping --------------------------------------------------------------------

test_that("usage escaping preserved when combined", {
  out <- roc_proc_text(rd_roclet(), "
    #' Foo
    foo <- function(x = '%') x

    #' @rdname foo
    bar <- function(y = '%') y
  ")[[1]]

  expect_is(out$get_value("usage"), "rd")
})

test_that("default usage not double escaped", {
  out <- roc_proc_text(rd_roclet(), "
    #' Regular
    mean.foo <- function(x) 'foo'
  ")[[1]]

  expect_equal(out$get_rd("usage"), "\\usage{\n\\method{mean}{foo}(x)\n}")
})

test_that("% and \\ are escaped in usage", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    a <- function(a='%\\\\') {}")[[1]]
  expect_equal(out$get_value("usage"), escape('a(a = "%\\\\")'))
  expect_equal(out$get_rd("usage"), "\\usage{\na(a = \"\\%\\\\\\\\\")\n}")
})

test_that("% and \\ not escaped in manual usage", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    #' @usage %\\
    a <- function(a) {}
  ")[[1]]
  expect_equal(out$get_value("usage"), rd('%\\'))
  expect_equal(out$get_rd("usage"), '\\usage{\n%\\\n}')
})

test_that("non-syntactic names are quoted", {

  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    'a b' <- function(x) x")[[1]]

  expect_equal(out$get_value("usage"), rd('"a b"(x)'))
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

  expect_equal(out$get_value("rcmethods"), list("draw(x = 1)" = "2"))
})
