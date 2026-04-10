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
  obj <- call_to_object(desc::desc_get)
  expect_s3_class(obj, "import")
  expect_equal(obj$alias, "desc_get")
  expect_equal(obj$value$pkg, "desc")
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

test_that("finds value created with assignment", {
  obj <- call_to_object({
    foo <- 1:10
  })
  expect_s3_class(obj, "value")
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


# S7 ----------------------------------------------------------------------

test_that("finds S7 classes", {
  skip_unless_r(">= 4.3.0")
  obj <- call_to_object({
    Dog <- S7::new_class(
      "Dog",
      properties = list(
        name = S7::class_character,
        age = S7::class_numeric
      )
    )
  })
  expect_s3_class(obj, "s7class")
  expect_equal(obj$topic, "Dog")
  expect_equal(obj$alias, "Dog")
})

test_that("finds S7 generics", {
  skip_unless_r(">= 4.3.0")
  obj <- call_to_object({
    speak <- S7::new_generic("speak", "x")
  })

  expect_s3_class(obj, "s7generic")
  expect_equal(obj$topic, "speak")
  expect_equal(obj$alias, "speak")
})

test_that("finds S7 methods", {
  skip_unless_r(">= 4.3.0")
  obj <- call_to_object({
    Dog <- S7::new_class("Dog")
    speak <- S7::new_generic("speak", "x")
    S7::method(speak, Dog) <- function(x) "Woof"
  })
  expect_s3_class(obj, "s7method")
  expect_equal(obj$topic, "speak,Dog-method")
  expect_equal(obj$value$generic, "speak")
  expect_equal(obj$value$classes, list("Dog"))
})

test_that("finds S7 multi-dispatch methods", {
  skip_unless_r(">= 4.3.0")
  obj <- call_to_object({
    Dog <- S7::new_class("Dog")
    Cat <- S7::new_class("Cat")
    greet <- S7::new_generic("greet", c("x", "y"))
    S7::method(greet, list(Dog, Cat)) <- function(x, y) "hi"
  })
  expect_s3_class(obj, "s7method")
  expect_equal(obj$topic, "greet,Dog,Cat-method")
  expect_equal(obj$value$classes, list("Dog", "Cat"))
})

test_that("S7 union method has aliases for individual classes", {
  skip_unless_r(">= 4.3.0")
  obj <- call_to_object({
    Dog <- S7::new_class("Dog")
    Cat <- S7::new_class("Cat")
    Pet <- S7::new_union(Dog, Cat)
    speak <- S7::new_generic("speak", "x")
    S7::method(speak, Pet) <- function(x) "hi"
  })
  expect_s3_class(obj, "s7method")
  expect_equal(obj$topic, "speak,Dog/Cat-method")
  expect_equal(obj$alias, c("speak,Dog-method", "speak,Cat-method"))
})

test_that("S7 non-union method has no extra aliases", {
  skip_unless_r(">= 4.3.0")
  obj <- call_to_object({
    Dog <- S7::new_class("Dog")
    speak <- S7::new_generic("speak", "x")
    S7::method(speak, Dog) <- function(x) "Woof"
  })
  expect_equal(obj$topic, "speak,Dog-method")
  # alias is _extra_ aliases, apart from topic name
  expect_null(obj$alias)
})

test_that("finds S7 methods for S3 generics", {
  skip_unless_r(">= 4.3.0")
  obj <- call_to_object({
    Dog <- S7::new_class("Dog")
    S7::method(print, Dog) <- function(x, ...) cat("Dog\n")
  })
  expect_s3_class(obj, "s7method")
  expect_equal(obj$topic, "print,Dog-method")
  expect_equal(obj$value$generic, "print")
})

test_that("S7 method topic includes package prefix in class name", {
  skip_unless_r(">= 4.3.0")
  obj <- call_to_object({
    Dog <- S7::new_class("Dog", package = "mypkg")
    speak <- S7::new_generic("speak", "x")
    S7::method(speak, Dog) <- function(x) "Woof"
  })
  expect_equal(obj$topic, "speak,mypkg::Dog-method")
  expect_equal(obj$value$classes, list("mypkg::Dog"))
})

test_that("S7 method on S3 generic includes package prefix in class name", {
  skip_unless_r(">= 4.3.0")
  obj <- call_to_object({
    Dog <- S7::new_class("Dog", package = "mypkg")
    S7::method(print, Dog) <- function(x, ...) cat("Dog\n")
  })
  expect_equal(obj$topic, "print,mypkg::Dog-method")
  expect_equal(obj$value$generic, "print")
})

test_that("S7 method with unknown class type warns", {
  skip_unless_r(">= 4.3.0")
  block <- roxy_block(tags = list(), file = "test.R", line = 1, call = quote(x))
  expect_snapshot(s7_class_name(42L, block))
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

# R6 $set() ---------------------------------------------------------------

test_that("recognises R6$set() calls", {
  obj <- call_to_object({
    C <- R6::R6Class("C", public = list(m = function() {}))
    C$set("public", "meth", function(x) {})
  })

  expect_s3_class(obj, "r6method")
  expect_equal(obj$value, list(class = "C", method = "meth"))
})

test_that("ignores non-R6 $ calls", {
  obj <- call_to_object({
    x <- list(set = function(...) {})
    x$set(x)
  })
  expect_null(obj)
})

test_that("ignores $set() with non-string method name", {
  obj <- call_to_object({
    C <- R6::R6Class("C", public = list(m = function() {}))
    name <- "meth"
    C$set("public", name, function(x) {})
  })
  expect_null(obj)
})
