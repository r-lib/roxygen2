context("Re-export")

test_that("exporting a call to :: produces re-exports documentation", {
  out <- roc_proc_text(rd_roclet(), "
    #' @export
    testthat::auto_test")[[1]]

  expect_equal(get_tag(out, "title")$value, "Objects exported from other packages")
  expect_equal(get_tag(out, "reexport")$value, list(pkg = "testthat", fun = "auto_test"))
  expect_equal(get_tag(out, "keyword")$value, "internal")
})
