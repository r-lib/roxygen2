context("Object: S4")

test_that("class captured from setClass", {
  out <- parse_text("
    #' Title
    setClass('A1')
  ")[[1]]

  object <- attr(out, "object")
  expect_is(object, "s4class")
  expect_equal(object$alias, NULL)
})

test_that("class captured from assignment", {
  out <- parse_text("
    #' Title
    B <- setClass('B1')
  ")[[1]]

  object <- attr(out, "object")
  expect_is(object, "s4class")
  expect_equal(object$alias, "B")
})

test_that("class captured from setClassUnion", {
  out <- parse_text("
    #' Title
    setClassUnion('numberish', c('numeric', 'logical'))
  ")[[1]]

  object <- attr(out, "object")
  expect_is(object, "s4class")
  expect_equal(object$alias, NULL)
})

test_that("class captured from setRefClass", {
  out <- parse_text("
    #' Title
    setRefClass('A1')
  ")[[1]]

  object <- attr(out, "object")
  expect_is(object, "rcclass")
  expect_equal(object$alias, NULL)
})

test_that("class captured from assignment of setRefClass", {
  out <- parse_text("
    #' Title
    B <- setRefClass('B1')
  ")[[1]]

  object <- attr(out, "object")
  expect_is(object, "rcclass")
  expect_equal(object$alias, "B")
})

test_that("setMethod equivalent to setReplaceMethod", {
  out <- parse_text("
    setGeneric('foo<-', function(x, value) standardGeneric('foo<-'))

    #' setMethod
    setMethod('foo<-', 'numeric', function(x, value) value * 10)
    #' setReplace
    setReplaceMethod('foo', 'numeric', function(x, value) value * 10)
    ")

  expect_equal(attr(out[[1]], "object"), attr(out[[2]], "object"))
})
