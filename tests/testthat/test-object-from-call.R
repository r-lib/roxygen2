
test_that("undocumentable things return null", {
  expect_null(object_from_call2(NULL))
  expect_null(object_from_call2(10))
  expect_null(object_from_call2(1 + 2))

  foo <- function() {
    function() {}
  }
  expect_null(object_from_call2(foo()))
  expect_null(object_from_call2(foo()()))
})


# functions ------------------------------------------------------------

test_that("finds function created with assignment", {
  obj <- object_from_call2({
    foo <- function(x, y, z) {}
  })
  expect_s3_class(obj, "function")
})

test_that("ignored compound assignment", {
  obj <- object_from_call2({
    foo <- list()
    foo[[1]] <- function(x, y, z) {}
  })
  expect_null(obj)
})

# S4 ----------------------------------------------------------------------

test_that("finds correct parser even when namespaced", {
  obj <- object_from_call2({
    setClass("Foo")
    setGeneric("bar", function(x) standardGeneric("bar"))
    methods::setMethod('bar', 'Foo', function(x) {})
  })
  expect_s3_class(obj, "s4method")
})

test_that("finds arguments when S4 method wrapped inside .local()", {
  obj <- object_from_call2({
    setClass("Foo")
    setMethod('subset', 'Foo', function(x, foo, ...) {})
  })
  expect_s3_class(obj, "s4method")
  expect_named(formals(obj$value@.Data), c("x", "foo", "..."))
})

# extract_method_fun ------------------------------------------------------

test_that("fails gracefully on bad inputs", {
  fun1 <- function() {}
  fun2 <- function() 1 + 2
  fun3 <- function() {
    1 + 2
  }
  fun4 <- function() {
    x <- 1
  }
  fun5 <- function() {
    .local <- 1
  }

  expect_equal(extract_method_fun(fun1), fun1)
  expect_equal(extract_method_fun(fun2), fun2)
  expect_equal(extract_method_fun(fun3), fun3)
  expect_equal(extract_method_fun(fun4), fun4)
  expect_equal(extract_method_fun(fun5), fun5)
})
