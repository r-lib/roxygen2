context("Re-export")

test_that("exporting a call to :: produces re-exports documentation", {
  out <- roc_proc_text(rd_roclet(), "
    #' @export
    testthat::auto_test")[[1]]

  expect_equal(get_tag(out, "title")$value, "Re-exported functions")
  expect_equal(get_tag(out, "reexport")$value, list(pkg = "testthat", fun = "auto_test"))
  expect_equal(get_tag(out, "keyword")$value, "internal")
})

test_that("can combine multiple rexports", {
  out <- roc_proc_text(rd_roclet(), "
    #' @export
    testthat::auto_test")[[1]]

  expect_equal(get_tag(out, "title")$value, "Re-exported functions")
  expect_equal(get_tag(out, "reexport")$value, list(pkg = "testthat", fun = "auto_test"))
  expect_equal(get_tag(out, "keyword")$value, "internal")
})
