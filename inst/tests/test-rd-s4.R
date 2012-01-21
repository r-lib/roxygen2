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

