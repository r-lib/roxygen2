test_that("@describeIn generic captures s3 method class", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    f <- function(x) UseMethod('f')

    #' @describeIn f Method for a
    #'
    f.a <- function(x) 1
  ")[[1]]

  expect_equal(out$get_value("minidesc")$method, TRUE)
  expect_equal(out$get_value("minidesc")$generic, "f")
  expect_equal(out$get_value("minidesc")$label, "a")
})

test_that("@describeIn generic captures s4 method class", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    setGeneric('f', function(x) standardGeneric('f'))

    #' @describeIn f Method for a
    setMethod(f, signature('a'), function(x) 1)
  ")[[1]]

  expect_equal(out$get_value("minidesc")$method, TRUE)
  expect_equal(out$get_value("minidesc")$generic, "f")
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

  expect_equal(out$get_value("minidesc")$method, TRUE)
  expect_equal(out$get_value("minidesc")$generic, "")
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

  expect_equal(out$get_value("minidesc")$method, TRUE)
  expect_equal(out$get_value("minidesc")$generic, "")
  expect_equal(out$get_value("minidesc")$label, "mean")
})

test_that("@describeIn class captures function name", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    f <- function(x) 1

    #' @describeIn f A
    f2 <- function(x) 1
    ")[[1]]

  expect_equal(out$get_value("minidesc")$method, FALSE)
  expect_equal(out$get_value("minidesc")$generic, "")
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
  out
  expect_match(out$get_rd("minidesc"), "\\\\%foo\\\\%")
})

test_that("minidescription is correctly formatted", {
  x <- rd_section_minidesc(
    df = subsection(label = c("foo", "bar"), desc = c("baz", "qux")),
    method = TRUE,
    generic = "corge"
  )
  expect_equal_strings(
    format(x),
    "\\section{Related Functions and Methods}{\n
    \\subsection{Methods extending \\code{corge} generic (by class):}{
    \n\\itemize{\n\\item \\code{foo}: baz\n\\item \\code{bar}: qux\n}\n
    }\n}\n"
  )
})

test_that("Multiple @describeIn functions combined into one", {
  example <- test_path("roxygen-block-describe-in-functions.R")
  out <- roc_proc_text(rd_roclet(), brio::read_file(example))[[1]]

  expect_equal(out$get_value("minidesc")$method, c(FALSE, FALSE))
  expect_equal(out$get_value("minidesc")$generic, c("", ""))
  expect_equal(out$get_value("minidesc")$label, c("square", "cube"))
})

test_that(
  "Multiple @describeIn methods and others are combined into a generic", {
    example <- test_path("roxygen-block-describe-in-method-in-gen.R")
    out <- roc_proc_text(rd_roclet(), brio::read_file(example))[[1]]
    expect_equal(out$get_value("minidesc")$method, c(TRUE, TRUE, FALSE, FALSE))
    expect_equal(out$get_value("minidesc")$generic, c("zap", "zap", "", ""))
    expect_equal(
      out$get_value("minidesc")$label,
      c("numeric", "character", "print.qux", "zap_helper")
    )
  }
)

test_that(
  "Multiple @describeIn methods and others are combined into a class constructor",
  {
    example <- test_path("roxygen-block-describe-in-method-in-const.R")
    out <- roc_proc_text(rd_roclet(), brio::read_file(example))[[1]]

    expect_equal(out$get_value("minidesc")$method, c(TRUE, TRUE, FALSE, FALSE))
    expect_equal(out$get_value("minidesc")$generic, rep("", 4))
    expect_equal(
      out$get_value("minidesc")$label, 
      c("print", "format", "format.bar", "is_foo")
    )
  }
)
