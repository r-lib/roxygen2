test_that("undocumentable things return null", {
  expect_null(call_to_object(NULL))
  expect_null(call_to_object(10))
  expect_null(call_to_object(1 + 2))
})

# data / package -------------------------------------------------------

test_that("recommends use of _PACKAGE", {
  path <- local_package_copy(test_path("empty"))

  block <- "
    #' @docType package
    NULL
  "
  withr::with_dir(path, expect_snapshot(out <- parse_text(block)[[1]]))

  expect_s3_class(out$object, "package")
  expect_equal(out$object$value$desc$get_field("Package"), "empty")
})

test_that("finds package description", {
  obj <- call_to_object("_PACKAGE", file = test_path("testEagerData/R/a.r"))
  expect_s3_class(obj, "package")
  expect_equal(obj$value$desc$get_field("Package"), "testEagerData")
})


test_that("finds datasets given by name", {
  obj <- call_to_object({
    df <- data.frame(x = 1, y = 2)
    "df"
  })
  expect_s3_class(obj, "data")
  expect_equal(obj$alias, "df")
  expect_s3_class(obj$value, "data.frame")
})

test_that("can document eager data", {
  path <- local_package_copy(test_path('testEagerData'))
  suppressMessages(roxygenise(path))
  withr::defer(pkgload::unload("testEagerData"))

  expect_true(file.exists(file.path(path, "man/a.Rd")))
})

test_that("can document lazy data", {
  path <- local_package_copy(test_path('testLazyData'))
  suppressMessages(roxygenise(path))
  withr::defer(pkgload::unload("testLazyData"))

  expect_true(file.exists(file.path(path, "man/a.Rd")))
})

# imports -----------------------------------------------------------------

test_that("find function imported from another package", {
  obj <- call_to_object(purrr::map_int)
  expect_s3_class(obj, "import")
  expect_equal(obj$alias, "map_int")
  expect_equal(obj$value$pkg, "purrr")
})

# assignment ------------------------------------------------------------

test_that("finds function created with assignment", {
  obj <- call_to_object({
    foo <- function(x, y, z) {}
  })
  expect_s3_class(obj, "function")
})

test_that("finds S3 generic created with assignment", {
  obj <- call_to_object({
    foo <- function(x, y, z) UseMethod("foo")
  })
  expect_s3_class(obj, "s3generic")
})

test_that("finds S3 method created with assignment", {
  obj <- call_to_object({
    foo <- function(x, y, z) UseMethod("foo")
    foo.method <- function(x, y, z) {}
  })
  expect_s3_class(obj, "s3method")
})

test_that("finds data created with assignment", {
  obj <- call_to_object({
    foo <- 1:10
  })
  expect_s3_class(obj, "data")
})

test_that("finds class generator", {
  obj <- call_to_object({
    newFoo <- setClass("Foo")
  })
  expect_s3_class(obj, "s4class")
  expect_equal(obj$alias, "newFoo")
  expect_s4_class(obj$value, "classRepresentation")

  obj <- call_to_object({
    newFoo <- setRefClass("Foo")
  })
  expect_s3_class(obj, "rcclass")
  expect_equal(obj$alias, "newFoo")
  expect_s4_class(obj$value, "classRepresentation")
})

test_that("ignored compound assignment", {
  obj <- call_to_object({
    foo <- list()
    foo[[1]] <- function(x, y, z) {}
  })
  expect_null(obj)
})

test_that("finds function created with delayed assignment", {
  obj <- call_to_object({
    delayedAssign("foo", function(x, y, z) {})
  })
  expect_s3_class(obj, "function")
})

# S3 ----------------------------------------------------------------------

test_that("can derive S3 metadata for base generics", {
  block <- "
    #' @export
    mean.foo <- function(...) 1
  "
  out <- parse_text(block)[[1]]

  expect_equal(s3_method_info(out$object$value), c("mean", "foo"))
})

test_that("@method overrides auto-detection", {
  block <- "
    #' @export
    #' @method all.equal data.frame
    all.equal.data.frame <- function(...) 1
  "
  out <- parse_text(block)[[1]]

  expect_equal(s3_method_info(out$object$value), c("all.equal", "data.frame"))
})

test_that("exportS3Method registers S3 metadata", {
  block <- "
    #' @exportS3Method stats::median
    median.foo <- function(...) 1
  "
  out <- parse_text(block)[[1]]
  expect_equal(s3_method_info(out$object$value), c("median", "foo"))
})


# S4 ----------------------------------------------------------------------

test_that("finds S4 and RC classes", {
  obj <- call_to_object(setClass("Foo"))
  expect_s3_class(obj, "s4class")
  expect_equal(obj$topic, "Foo-class")
  expect_equal(obj$alias, NULL)

  obj <- call_to_object(setRefClass("Foo"))
  expect_s3_class(obj, "rcclass")
  expect_equal(obj$topic, "Foo-class")

  obj <- call_to_object({
    setClass("Foo")
    setClassUnion("Foo2", "Foo")
  })
  expect_s3_class(obj, "s4class")
  expect_equal(obj$topic, "Foo2-class")
})

test_that("finds S4 generics and methods", {
  obj <- call_to_object({
    setGeneric("bar", function(x) standardGeneric("bar"))
  })
  expect_s3_class(obj, "s4generic")

  obj <- call_to_object({
    setGeneric("bar", function(x) standardGeneric("bar"))
    setMethod('bar', 'Foo', function(x) {})
  })
  expect_s3_class(obj, "s4method")

  obj <- call_to_object({
    setGeneric("bar<-", function(x, value) standardGeneric("bar<-"))
    setReplaceMethod("bar", "Foo", function(x, value) {})
  })
  expect_s3_class(obj, "s4method")
})

test_that("finds correct parser even when namespaced", {
  obj <- call_to_object({
    setClass("Foo")
    setGeneric("baz", function(x) standardGeneric("baz"))
    methods::setMethod('baz', 'Foo', function(x) {})
  })
  expect_s3_class(obj, "s4method")
})

test_that("finds arguments when S4 method wrapped inside .local()", {
  obj <- call_to_object({
    setClass("Foo")
    setMethod('subset', 'Foo', function(x, foo, ...) {})
  })
  expect_s3_class(obj, "s4method")
  expect_named(formals(obj$value@.Data), c("x", "foo", "..."))
})


# R.oo / R.methodsS3 ------------------------------------------------------

test_that("can define constructor with R.oo", {
  skip_if_not_installed("R.oo")
  obj <- call_to_object({
    R.oo::setConstructorS3("Foo", function(x, y, z) {})
  })
  expect_s3_class(obj, "function")
  expect_equal(obj$alias, "Foo")
})

test_that("can define method for R.methodsS3", {
  skip_if_not_installed("R.methodsS3")
  obj <- call_to_object({
    R.methodsS3::setMethodS3("foo", "default", function(x, ...) {})
  })
  expect_s3_class(obj, "s3method")
  expect_equal(obj$alias, "foo.default")
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
