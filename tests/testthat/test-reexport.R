context("Re-export")

test_that("exporting a call to :: produces re-exports documentation", {
  out <- roc_proc_text(rd_roclet(), "
    #' @export
    testthat::auto_test")[[1]]

  expect_equal(
    out$get_field("reexport"),
    roxy_field_reexport("testthat", "auto_test")
  )

  expect_equal(
    out$get_field("title")$value,
    "Objects exported from other packages"
  )

  expect_equal(out$get_field("keyword")$value, "internal")
})

test_that("multiple re-exports are combined", {
  out <- roc_proc_text(rd_roclet(), "
    #' @export
    testthat::expect_lt

    #' @export
    testthat::expect_gt
    ")[[1]]

  expect_equal(
    out$get_field("reexport"),
    roxy_field_reexport(c("testthat", "testthat"), c("expect_lt", "expect_gt"))
  )
})
