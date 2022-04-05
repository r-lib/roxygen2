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

test_that("Multiple @describeIn generic combined into one", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    f <- function(x) UseMethod('f')

    #' @describeIn f A
    f.a <- function(x) 1

    #' @describeIn f B
    f.b <- function(x) 1
  ")[[1]]

  expect_equal(out$get_value("minidesc")$type, "generic")
  expect_equal(out$get_value("minidesc")$label, c("a", "b"))
  expect_equal(out$get_value("minidesc")$desc, c("A", "B"))
})


test_that("all other combinations fallback to function list", {
  out <- roc_proc_text(rd_roclet(), "
    #' Generic
    foo <- function(x) UseMethod('foo')

    #' @describeIn foo related function
    bar <- function(y) {}
  ")[[1]]

  expect_equal(out$get_value("minidesc")$type, "function")
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

test_that("function names are escaped", {
  out <- roc_proc_text(rd_roclet(), "
    #' foo
    foo <- 100

    #' @describeIn foo shortcut for foo
    `%foo%` <- function(x, y) foo(x, y)
    ")[[1]]
  expect_match(out$get_rd("minidesc"), "\\\\%foo\\\\%")
})


test_that("complains about bad usage", {
  expect_snapshot_warning(
    roc_proc_text(rd_roclet(), "
      #' bar
      bar <- 100

      #' @name bar
      #' @describeIn foo shortcut for foo
      NULL
      "
    )
  )
  expect_snapshot_warning(
    roc_proc_text(rd_roclet(), "
      #' bar
      bar <- 100

      #' @name bar
      #' @describeIn foo shortcut for foo
      foo <- 10
      "
    )
  )
  expect_snapshot_warning(
    roc_proc_text(rd_roclet(), "
      #' bar
      bar <- 100

      #' @rdname bar
      #' @describeIn foo shortcut for foo
      foo <- 10
      "
    )
  )
})
