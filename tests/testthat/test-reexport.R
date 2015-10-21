context("Re-export")

test_that("@param documents arguments", {
  out <- roc_proc_text(rd_roclet(), "
    #' @export
    testthat::auto_test")[[1]]

  expect_equal(get_tag(out, "title")$value, "Re-exported functions")
  expect_equal(get_tag(out, "keyword")$value, "internal")
})
