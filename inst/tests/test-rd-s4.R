context("Rd - S4")
roc <- rd_roclet()

test_that("Method documentation has correct defaults", {
  out <- roc_proc_text(roc, "
    #' Title.
    setMethod('show', 'numeric', function(object){ show(NA) })
    ")[[1]]

  expect_equal(get_tag(out, "alias")$values, "show,numeric-method")
  expect_equal(names(get_tag(out, "arguments")$values), c("object"))

})

test_that("generic documentation generated correctly", {
  out <- roc_proc_text(roc, "
    #' My foo function.
    #'
    #' Foo will have S4 methods
    #' @param object my object
    setGeneric('foo', function(object){
        standardGeneric('foo')
    })    
    ")[[1]]
  expect_equal(get_tag(out, "usage")$values, "foo(object)")
  expect_equal(get_tag(out, "alias")$values, c("foo", "foo-methods"))
})


test_that("@usage for S4methods", {
  out <- roc_proc_text(roc, "
    #' Title.
    setMethod('show', signature = c(object = 'array'), function (object) {})
    ")[[1]]
  expect_equal(get_tag(out, "usage")$values,
    "\\S4method{show}{array}(object)")
  expect_equal(get_tag(out, "alias")$values,
    c("show,array-method"))
})