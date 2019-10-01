test_that("exporting a call to :: produces re-exports documentation", {
  out <- roc_proc_text(rd_roclet(), "
    #' @export
    testthat::auto_test")[[1]]

  expect_equal(
    out$get_section("reexport"),
    rd_section_reexport("testthat", "auto_test")
  )
  expect_equal(out$get_value("title"), "Objects exported from other packages")
  expect_equal(out$get_value("keyword"), "internal")

  verify_output(test_path("test-object-import.txt"), cat(format(out)))
})

test_that("multiple re-exports are combined", {
  out <- roc_proc_text(rd_roclet(), "
    #' @export
    testthat::expect_lt

    #' @export
    testthat::expect_gt
    ")[[1]]

  expect_equal(
    out$get_section("reexport"),
    rd_section_reexport(c("testthat", "testthat"), c("expect_lt", "expect_gt"))
  )
})

test_that("description generated correctly", {
  roc <- rd_roclet()
  out <- roc_proc_text(rd_roclet(), "
    #' @importFrom magrittr %>%
    #' @export
    magrittr::`%>%`
    ")[[1]]

  expect_null(out$get_section("description"))
})

test_that("can't set description and re-export", {
  expect_warning(
    out <- roc_proc_text(rd_roclet(), "
      #' @description NOPE
      #' @export
      magrittr::`%>%`
      "),
    "Can't use description when re-exporting"
  )

  expect_length(out, 0)
})
