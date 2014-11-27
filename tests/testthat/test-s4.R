context("S4")

test_that("class captured from setClass", {
  out <- parse_text("
    #' Title
    setClass('A')
  ")$blocks[[1]]

  expect_is(out$object, "s4class")
  expect_equal(out$object$alias, NULL)
})

test_that("class captured from assignment", {
  out <- parse_text("
    #' Title
    B <- setClass('B')
  ")$blocks[[1]]

  expect_is(out$object, "s4class")
  expect_equal(out$object$alias, "B")
})

test_that("class captured from setRefClass", {
  out <- parse_text("
    #' Title
    setRefClass('A')
  ")$blocks[[1]]

  expect_is(out$object, "rcclass")
  expect_equal(out$object$alias, NULL)
})

test_that("class captured from assignment of setRefClass", {
  out <- parse_text("
    #' Title
    B <- setRefClass('B')
  ")$blocks[[1]]

  expect_is(out$object, "rcclass")
  expect_equal(out$object$alias, "B")
})


test_that("setMethod equivalent to setReplaceMethod", {
  out <- parse_text("
    setGeneric('foo<-', function(x, value) standardGeneric('foo<-'))

    #' setMethod
    setMethod('foo<-', 'numeric', function(x, value) value * 10)
    #' setReplace
    setReplaceMethod('foo', 'numeric', function(x, value) value * 10)
    ")$blocks

  expect_equal(out[[2]]$object, out[[3]]$object)
})
