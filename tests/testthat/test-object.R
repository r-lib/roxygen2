context("Object")

# Generate one object of every type --------------------------------------------

env <- pkg_env()

# S3
h <- function(x) UseMethod("h")
h1 <- add_s3_metadata(h, "h", environment())
h.A <- function(x) 1
h1.A <- add_s3_metadata(h.A, "h.A", environment())

# S4
B <- setClass("B", contains = "list", where = env)
classB <- getClass("B", where = env)
setGeneric("g", function(x, ...) standardGeneric("g"), where = env)
g <- getGeneric("g", where = env)

setMethod("g", "B", function(x, ...) 1, where = env)
method1 <- getMethod("g", "B", where = env)

setClass("C", contains = "B", where = env)
setMethod("g", "C", function(x, ..., blah = 7) 1, where = env)
method2 <- getMethod("g", "C", where = env)

# RC
A <- setRefClass("A", where = env, methods = list(f = function() 1))
classA <- getClass("A", where = env)

# Object type ------------------------------------------------------------------

test_that("obj_type correctly classifies objects", {
  expect_equal(obj_type(h1), "s3generic")
  expect_equal(obj_type(h1.A), "s3method")

  expect_equal(obj_type(classB), "s4class")
  expect_equal(obj_type(g), "s4generic")
  expect_equal(obj_type(method1), "s4method")
  expect_equal(obj_type(method2), "s4method")

  expect_equal(obj_type(classA), "rcclass")
})

# Object standardisation -------------------------------------------------------

test_that("generators standardised to classes", {
  objA <- standardise_obj("A", A, env = env)
  objB <- standardise_obj("B", B, env = env)

  expect_is(objA, "classRepresentation")
  expect_is(objB, "classRepresentation")
})

test_that("methods with extra args get correct formals", {
  obj <- standardise_obj("method2", method2, env = env)
  args <- formals(obj@.Data)

  expect_equal(names(args), c("x", "...", "blah"))
})

test_that("s3 generics and methods labelled with metadata", {
  generic <- standardise_obj("h", h, env = environment())
  method  <- standardise_obj("h.A", h.A, env = environment())

  expect_is(generic, "s3generic")
  expect_is(method, "s3method")
})

# Object creation --------------------------------------------------------------

test_that("generators get function name as alias", {
  objA <- object(standardise_obj("A", A, env = env), "genA")
  expect_equal(objA$alias, "genA")

  objB <- object(standardise_obj("B", B, env = env), "genB")
  expect_equal(objB$alias, "genB")
})

test_that("refclasses get rc_methods listing", {
  objA <- object(standardise_obj("A", A, env = env), "genA")
  expect_equal(names(objA$methods), "f")
})

removeClass("A", where = env)
removeClass("B", where = env)
removeClass("C", where = env)
