test_that("@describeIn generic destination captures s3 method source", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    f <- function(x) UseMethod('f')

    #' @describeIn f Method for a
    #'
    f.a <- function(x) 1
  ")[[1]]
  expect_equal(out$get_value("minidesc")$extends, "generic")
  expect_equal(out$get_value("minidesc")$generic, "f")
  expect_equal(out$get_value("minidesc")$class, "a")
})

test_that("@describeIn generic destination captures s4 method source", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    setGeneric('f', function(x) standardGeneric('f'))

    #' @describeIn f Method for a
    setMethod(f, signature('a'), function(x) 1)
  ")[[1]]
  out$get_value("minidesc")

  expect_equal(out$get_value("minidesc")$extends, "generic")
  expect_equal(out$get_value("minidesc")$generic, "f")
  expect_equal(out$get_value("minidesc")$class, "a")
})

test_that("@describeIn constructor destination captures s3 method source", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    boo <- function() structure(list(), class = 'boo')

    #' @describeIn boo mean method
    #'
    mean.boo <- function(x) 1
    ")[[1]]

  expect_equal(out$get_value("minidesc")$extends, "class")
  expect_equal(out$get_value("minidesc")$generic, "mean")
  expect_equal(out$get_value("minidesc")$class, "boo")
})

test_that("@describeIn constructor destination captures s4 method source", {
  out <- roc_proc_text(rd_roclet(), "
    setGeneric('mean')

    #' Title
    setClass('a')

    #' @describeIn a mean method
    setMethod('mean', 'a', function(x) 1)
    ")[[1]]
  out$get_value("minidesc")
  expect_equal(out$get_value("minidesc")$extends, "class")
  expect_equal(out$get_value("minidesc")$generic, "mean")
  expect_equal(out$get_value("minidesc")$class, "a")
})

test_that("@describeIn function destination captures function source", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    f <- function(x) 1

    #' @describeIn f A
    f2 <- function(x) 1
    ")[[1]]

  expect_equal(out$get_value("minidesc")$name, "f2")
  expect_equal(out$get_value("minidesc")$extends, "")
  expect_equal(out$get_value("minidesc")$generic, "")
  expect_equal(out$get_value("minidesc")$class, "")
})

test_that("@describeIn class captures function name with data", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #' @name f
    NULL

    #' @describeIn f A
    f2 <- function(x) 1
    ")[[1]]

  expect_equal(out$get_value("minidesc")$name, "f2")
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

test_that("Multiple @describeIn functions combined into one", {
  example <- test_path("roxygen-block-describe-in-functions.R")
  out <- roc_proc_text(rd_roclet(), brio::read_file(example))[[1]]

  expect_equal(out$get_value("minidesc")$name, c("square", "cube"))
  expect_equal(out$get_value("minidesc")$extends, c("", ""))
  expect_equal(out$get_value("minidesc")$generic, c("", ""))
  expect_equal(out$get_value("minidesc")$class, c("", ""))
})

test_that(
  "Multiple @describeIn methods and others are combined into a generic", {
    example <- test_path("roxygen-block-describe-in-method-in-gen.R")
    out <- roc_proc_text(rd_roclet(), brio::read_file(example))[[1]]
    expect_equal(
      out$get_value("minidesc")$extends,
      c("generic", "generic", "", "")
    )
    expect_equal(out$get_value("minidesc")$generic, c("zap", "zap", "print", ""))
    expect_equal(
      out$get_value("minidesc")$class, 
      c("numeric", "character", "qux", "")
    )
  }
)

test_that(
  "Multiple @describeIn methods and others are combined into a class constructor",
  {
    example <- test_path("roxygen-block-describe-in-method-in-const.R")
    out <- roc_proc_text(rd_roclet(), brio::read_file(example))[[1]]

    expect_equal(out$get_value("minidesc")$extends, c("class", "class", "", ""))

    # more complicated with disambiguated class name "pkg_class"
    out2 <- roc_proc_text(rd_roclet(), brio::read_file(example))[[2]]
    expect_equal(out2$get_value("minidesc")$extends, c("class", ""))
  }
)


test_that("function names are escaped", {
  out <- roc_proc_text(rd_roclet(), "
    #' foo
    foo <- 100

    #' @describeIn foo shortcut for foo
    `%foo%` <- function(x, y) foo(x, y)
    ")[[1]]
  out
  expect_match(out$get_rd("minidesc"), "\\\\%foo\\\\%")
})
