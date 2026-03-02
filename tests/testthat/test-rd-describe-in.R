test_that("@describeIn suggests @rdname", {
  block <- "
    #' @describeIn foo
    NULL
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))
})

test_that("@describeIn generic destination captures s3 method source", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    f <- function(x) UseMethod('f')

    #' @describeIn f Method for a
    #'
    f.a <- function(x) 1
  "
  )[[1]]
  expect_equal(out$get_value("minidesc")$extends, "generic")
  expect_equal(out$get_value("minidesc")$generic, "f")
  expect_equal(out$get_value("minidesc")$class, "a")
})

test_that("@describeIn generic destination captures s4 method source", {
  suppressMessages(
    out <- roc_proc_text(
      rd_roclet(),
      "
      #' Title
      setGeneric('f', function(x) standardGeneric('f'))

      #' @describeIn f Method for a
      setMethod(f, signature('a'), function(x) 1)
    "
    )[[1]]
  )
  out$get_value("minidesc")

  expect_equal(out$get_value("minidesc")$extends, "generic")
  expect_equal(out$get_value("minidesc")$generic, "f")
  expect_equal(out$get_value("minidesc")$class, "a")
})

test_that("@describeIn constructor destination captures s3 method source", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    boo <- function() structure(list(), class = 'boo')

    #' @describeIn boo mean method
    #'
    mean.boo <- function(x) 1
    "
  )[[1]]

  expect_equal(out$get_value("minidesc")$extends, "class")
  expect_equal(out$get_value("minidesc")$generic, "mean")
  expect_equal(out$get_value("minidesc")$class, "boo")
})

test_that("@describeIn constructor destination captures s4 method source", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    setGeneric('mean')

    #' Title
    setClass('a')

    #' @describeIn a mean method
    setMethod('mean', 'a', function(x) 1)
    "
  )[[1]]
  out$get_value("minidesc")
  expect_equal(out$get_value("minidesc")$extends, "class")
  expect_equal(out$get_value("minidesc")$generic, "mean")
  expect_equal(out$get_value("minidesc")$class, "a")
})

test_that("@describeIn function destination captures function source", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    f <- function(x) 1

    #' @describeIn f A
    f2 <- function(x) 1
    "
  )[[1]]

  expect_equal(out$get_value("minidesc")$name, "f2()")
  expect_equal(out$get_value("minidesc")$extends, "")
  expect_equal(out$get_value("minidesc")$generic, "")
  expect_equal(out$get_value("minidesc")$class, "")
})

test_that("@describeIn class captures function name with data", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #' @name f
    NULL

    #' @describeIn f A
    f2 <- function(x) 1
    "
  )[[1]]

  expect_equal(out$get_value("minidesc")$name, "f2()")
})

test_that("@describeIn class captures function description", {
  out <- roc_proc_text(
    rd_roclet(),
    "
  #' Title
  f <- function(x) 1

  #' @describeIn f A
  f2 <- function(x) 1
  "
  )[[1]]

  expect_equal(out$get_value("minidesc")$desc, "A")
})

test_that("Multiple @describeIn functions combined into one", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Power
    #' @param x base
    #' @param exp exponent
    power <- function(x, exp) x ^ exp

    #' @describeIn power Square a number
    square <- function(x) power(x, 2)

    #' @describeIn power Cube a number
    cube <- function(x) power(x, 3)
    "
  )[[1]]

  expect_snapshot(out$get_section("minidesc"))
})

test_that("multiple methods and others are combined into a generic", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Zap generic
    #'
    #' @param x Object to zap.
    zap <- function(x) UseMethod('zap')

    #' @describeIn zap method
    zap.numeric <- function(x) {}

    #' @describeIn zap method
    zap.character <- function(x) {}

    #' @describeIn zap function (method for different generic)
    print.qux <- function(x) {}

    #' @describeIn zap function
    zap_helper <- function(x) {}
    "
  )[[1]]

  expect_snapshot(out$get_section("minidesc"))
})

test_that("multiple methods and others are combined into a class constructor", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Class constructor
    #'
    #' @param x x
    foo <- function(x) {}

    #' @describeIn foo method
    print.foo <- function(x) {}

    #' @describeIn foo method
    format.foo <- function(x) {}

    #' @describeIn foo function (method for different class)
    format.bar <- function(x) {}

    #' @describeIn foo function
    is_foo <- function(x) {}
  "
  )[[1]]
  expect_snapshot(out$get_section("minidesc"))

  # more complicated with disambiguated class name "pkg_class"
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Class constructor with disambiguated class
    #'
    #' @param x x
    baz <- function(x) {}

    #' @describeIn baz method
    print.roxygen2_baz <- function(x) {}

    #' @describeIn baz function (method for another class)
    format.quuz_baz <- function(x) {}"
  )[[1]]
  expect_snapshot(out$get_section("minidesc"))
})

test_that("infix and replacement names get nice label", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' foo
    foo <- 100

    #' @describeIn foo infix
    `%foo%` <- function(x, y) foo(x, y)

    #' @describeIn foo replacement for foo
    `foo<-` <- function(x, value) 10

    "
  )[[1]]
  expect_snapshot(out$get_section("minidesc"))
})

test_that("s4 methods get nice label", {
  withr::defer(removeClass("foo1"))
  out <- roc_proc_text(
    rd_roclet(),
    "
      #' Class
      #'
      setClass('foo1', slots = c(id = 'character'), where = .GlobalEnv)

      #' @describeIn foo1 generic
      setGeneric('m_id', function(x) standardGeneric('m_id'))

      #' @describeIn foo1 function
      setMethod('m_id', 'foo1', function(x) x@id)
    "
  )[[1]]
  expect_snapshot(out$get_section("minidesc"))

  withr::defer(removeClass("foo2"))
  withr::defer(removeClass("foo3"))
  out <- roc_proc_text(
    rd_roclet(),
    "
    setClass('foo2', where = .GlobalEnv)
    setClass('foo3', where = .GlobalEnv)

    #' bar1
    setGeneric('bar1', function(x, y) standardGeneric('bar1'))

    #' @describeIn bar1 method1
    setMethod('bar1', c('foo2', 'foo3'), function(x, y) 1)
    #' @describeIn bar1 method2
    setMethod('bar1', c('foo3', 'foo2'), function(x, y) 1)
  "
  )[[1]]
  expect_snapshot(out$get_section("minidesc"))
})

test_that("complains about bad usage", {
  block <- "
    #' bar
    bar <- 100

    #' @name bar
    #' @describeIn foo shortcut for foo
    NULL
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))

  block <- "
    #' bar
    bar <- 100

    #' @name bar
    #' @describeIn foo shortcut for foo
    foo <- 10
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))

  block <- "
    #' bar
    bar <- 100

    #' @rdname bar
    #' @describeIn foo shortcut for foo
    foo <- 10
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))
})
