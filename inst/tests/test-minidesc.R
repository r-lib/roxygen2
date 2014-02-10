context("minidesc")

test_that("@minidesc generic captures s3 method class", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    f <- function(x) UseMethod('f')

    #' @rdname f
    #' @minidesc generic Method for a
    f.a <- function(x) 1
  ")[[1]]

  expect_equal(get_tag(out, "minidesc")$values$label, "a")
})

test_that("@minidesc generic captures s4 method class", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    setGeneric('f', function(x) standardGeneric('f'))

    #' @rdname f
    #' @minidesc generic Method for a
    setMethod(f, signature('a'), function(x) 1)
  ")[[1]]

  expect_equal(get_tag(out, "minidesc")$values$label, "a")
})

test_that("@minidesc class captures s3 generic name", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    a <- function() structure(list(), class = 'a')

    #' @rdname a
    #' @minidesc class mean method
    mean.a <- function(x) 1
    ")[[1]]

  expect_equal(get_tag(out, "minidesc")$values$label, "mean")
})

test_that("@minidesc class captures s4 generic name", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    setClass('a')

    #' @rdname a-class
    #' @minidesc class mean method
    setMethod('mean', 'a', function(x) 1)
    ")[[1]]

  expect_equal(get_tag(out, "minidesc")$values$label, "mean")
})


test_that("Multiple @minidesc generic combined into one", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    f <- function(x) UseMethod('f')

    #' @rdname f
    #' @minidesc generic A
    f.a <- function(x) 1

    #' @rdname f
    #' @minidesc generic B
    f.b <- function(x) 1
  ")[[1]]

  expect_equal(get_tag(out, "minidesc")$values$label, c("a", "b"))
  expect_equal(get_tag(out, "minidesc")$values$desc, c("A", "B"))
})
