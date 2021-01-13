test_that("@describeIn generic captures s3 method class", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    f <- function(x) UseMethod('f')

    #' @describeIn f Method for a
    #'
    f.a <- function(x) 1
  ")[[1]]

  expect_equal(out$get_value("minidesc")$type, "generic")
  expect_equal(out$get_value("minidesc")$label, "a")
})

test_that("@describeIn generic captures s4 method class", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    setGeneric('f', function(x) standardGeneric('f'))

    #' @describeIn f Method for a
    setMethod(f, signature('a'), function(x) 1)
  ")[[1]]

  expect_equal(out$get_value("minidesc")$label, "a")
})

test_that("@describeIn class captures s3 generic name", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    boo <- function() structure(list(), class = 'boo')

    #' @describeIn boo mean method
    #'
    mean.boo <- function(x) 1
    ")[[1]]

  expect_equal(out$get_value("minidesc")$label, "mean")
})

test_that("@describeIn class captures s4 generic name", {
  out <- roc_proc_text(rd_roclet(), "
    setGeneric('mean')

    #' Title
    setClass('a')

    #' @describeIn a mean method
    setMethod('mean', 'a', function(x) 1)
    ")[[1]]

  expect_equal(out$get_value("minidesc")$label, "mean")
})

test_that("@describeIn class captures function name", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    f <- function(x) 1

    #' @describeIn f A
    f2 <- function(x) 1
    ")[[1]]

  expect_equal(out$get_value("minidesc")$label, "f2")
})

test_that("@describeIn class captures function name with data", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #' @name f
    NULL

    #' @describeIn f A
    f2 <- function(x) 1
    ")[[1]]

  expect_equal(out$get_value("minidesc")$label, "f2")
})

test_that("@describeIn class captures function description", {
  out <- roc_proc_text(rd_roclet(), "
  #' Title
  f <- function(x) 1

  #' @describeIn f A
  f2 <- function(x) 1
  ")[[1]]

  expect_equal(out$get_value("minidesc")$desc, "A")
})

test_that("function names are escaped", {
  out <- roc_proc_text(rd_roclet(), "
    #' foo
    foo <- 100

    #' @describeIn foo shortcut for foo
    `%foo%` <- function(x, y) foo(x, y)
    ")[[1]]
  expect_match(out$get_rd("minidesc"), "\\\\%foo\\\\%")
})

test_that("Multiple @describeIn functions combined into one", {
  out <- roc_proc_text(
    rd_roclet(),
    brio::read_file(test_path("roxygen-block-describe-in-functions.R"))
  )[[1]]
  expect_equal(out$get_value("minidesc")$type, c("function", "function"))
  expect_equal(out$get_value("minidesc")$label, c("square", "cube"))
})

test_that(
  "Multiple @describeIn methods for external generics and functions combined into one", 
  {
    out <- roc_proc_text(
      rd_roclet(),
      brio::read_file(test_path("roxygen-block-describe-in-by-generic.R"))
    )[[1]]

    expect_equal(out$get_value("minidesc")$type, c("class", "class", "function"))
    expect_equal(out$get_value("minidesc")$label, c("print", "format", "is_foo"))
  }
)

test_that(
  "Multiple @describeIn methods for internal generics, external generics and functions combined into one", 
  {
    out <- roc_proc_text(
      rd_roclet(),
      brio::read_file(test_path("roxygen-block-describe-in-by-class.R"))
    )[[1]]

    expect_equal(out$get_value("minidesc")$type, c("generic", "generic", "generic", "function"))
    expect_equal(out$get_value("minidesc")$label, c("numeric", "character", "qux", "zap_helper"))
  }
)
