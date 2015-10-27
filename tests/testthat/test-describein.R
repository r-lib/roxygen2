context("describeIn")

test_that("@describeIn generic captures s3 method class", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    f <- function(x) UseMethod('f')

    #' @describeIn f Method for a
    #'
    f.a <- function(x) 1
  ")[[1]]

  expect_equal(get_tag(out, "minidesc")$values$type, "generic")
  expect_equal(get_tag(out, "minidesc")$values$label, "a")
})

test_that("@describeIn generic captures s4 method class", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    setGeneric('f', function(x) standardGeneric('f'))

    #' @describeIn f Method for a
    setMethod(f, signature('a'), function(x) 1)
  ")[[1]]

  expect_equal(get_tag(out, "minidesc")$values$label, "a")
})

test_that("@describeIn class captures s3 generic name", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    boo <- function() structure(list(), class = 'boo')

    #' @describeIn boo mean method
    #'
    mean.boo <- function(x) 1
    ")[[1]]

  expect_equal(get_tag(out, "minidesc")$values$label, "mean")
})

test_that("@describeIn class captures s4 generic name", {
  out <- roc_proc_text(rd_roclet(), "
    setGeneric('mean')

    #' Title
    setClass('a')

    #' @describeIn a mean method
    setMethod('mean', 'a', function(x) 1)
    ")[[1]]

  expect_equal(get_tag(out, "minidesc")$values$label, "mean")
})

test_that("Multiple @describeIn generic combined into one", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    f <- function(x) UseMethod('f')

    #' @describeIn f A
    f.a <- function(x) 1

    #' @describeIn f B
    #'
    f.b <- function(x) 1
  ")[[1]]

  expect_equal(get_tag(out, "minidesc")$values$type, "generic")
  expect_equal(get_tag(out, "minidesc")$values$label, c("a", "b"))
  expect_equal(get_tag(out, "minidesc")$values$desc, c("A", "B"))
})

test_that("@describeIn class captures function name", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    f <- function(x) 1

    #' @describeIn f A
    f2 <- function(x) 1
    ")[[1]]

  expect_equal(get_tag(out, "minidesc")$values$label, "f2")
})

test_that("@describeIn class captures function name with data", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #' @name f
    NULL

    #' @describeIn f A
    f2 <- function(x) 1
    ")[[1]]

  expect_equal(get_tag(out, "minidesc")$values$label, "f2")
})
