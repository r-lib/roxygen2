context("Namespace")

expect_namespace <- function(roxygen, expected) {
  output <- capture_roclet_output(make.namespace.roclet(), roxygen)
  expect_equal(output, expected)
}

test_that("export detects object name", {
  expect_namespace("#' @export
                          a <- 2",
                         expected='export(a)')  
})

test_that("export parameter overrides default", {
  expect_namespace("#' @export b
                          a <- 2",
                         expected='export(b)')  
})

test_that("export detects S4 class", {
  expect_namespace("#' @export
                          setClass('a')",
                         expected='exportClasses(a)')  
})


test_that("exportClass overrides default class name", {
  expect_namespace("#' @exportClass b
                          setClass('a')",
                         expected='exportClasses(b)')  
})

test_that("export detects method name", {
  expect_namespace("#' @export
                          setMethod('b', 'a')",
                         expected='exportMethods(b)')  
})

test_that("exportMethod overrides default method name", {
  expect_namespace("#' @exportMethod c
                          setMethod('b', 'a')",
                         expected='exportMethods(c)')  
})

test_that("other namespace tags produce correct output", {
  expect_namespace("#' @exportPattern test
                          #' @S3method test test
                          #' @import test
                          #' @importFrom test test
                          #' @importClassesFrom test test
                          #' @importMethodsFrom test test
                          roxygen()",
                         expected=c("exportPattern(test)",
                           "S3method(test, test)",
                           "import(test)",
                           "importFrom(test, test)",
                           "importClassesFrom(test, test)",
                           "importMethodsFrom(test, test)"))  
})
