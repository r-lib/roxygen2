context("Namespace")
roc <- namespace_roclet()

test_that("export detects object name", {
  out <- roc_proc_text(roc, "#' @export\na <- function(){}")
  expect_equal(out, 'export(a)')
})

test_that("export parameter overrides default", {
  out <- roc_proc_text(roc, "#' @export b\na <- function(){}")
  expect_equal(out, 'export(b)')
})

test_that("export detects S4 class", {
  out <- roc_proc_text(roc, "#' @export\nsetClass('a')")
  expect_equal(out, 'exportClasses(a)')
})

test_that("exportClass overrides default class name", {
  out <- roc_proc_text(roc, "#' @exportClass b\nsetClass('a')")
  expect_equal(out, 'exportClasses(b)')
})

test_that("export detects method name", {
  out <- roc_proc_text(roc, "
    #' @export\n
    setMethod('max', 'a', function(x, ...) x[1])")
  expect_equal(out, 'exportMethods(max)')  
})

test_that("exportMethod overrides default method name", {
  out <- roc_proc_text(roc, "
    #' @exportMethod c
    setMethod('max', 'a', function(x, ...) x[1])")
  expect_equal(out, 'exportMethods(c)')  
})

test_that("other namespace tags produce correct output", {
  out <- roc_proc_text(roc, "
    #' @exportPattern test
    #' @S3method test test
    #' @import test
    #' @importFrom test test
    #' @importClassesFrom test test
    #' @importMethodsFrom test test
    NULL")

  expect_equal(sort(out), sort(c("exportPattern(test)", "S3method(test,test)",
    "import(test)", "importFrom(test,test)", "importClassesFrom(test,test)",
    "importMethodsFrom(test,test)")))
})
