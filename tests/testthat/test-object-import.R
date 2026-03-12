test_that("exporting a call to :: produces re-exports documentation", {
  block <- "
    #' @export
    testthat::auto_test
  "
  out <- roc_proc_text(rd_roclet(), block)[[1]]

  expect_equal(
    out$get_section("reexport"),
    rd_section_reexport("testthat", "auto_test")
  )
  expect_equal(out$get_value("title"), "Objects exported from other packages")
  expect_equal(out$get_value("keyword"), "internal")
  expect_snapshot_output(cat(format(out)))

  # And generates correct namespace definitions
  out <- roc_proc_text(namespace_roclet(), block)
  expect_equal(out, c("export(auto_test)", "importFrom(testthat,auto_test)"))
})

test_that("multiple re-exports are combined", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @export
    testthat::expect_lt

    #' @export
    testthat::expect_gt
    "
  )[[1]]

  expect_equal(
    out$get_section("reexport"),
    rd_section_reexport(
      c("testthat", "testthat"),
      c("expect_lt", "expect_gt")
    )
  )
})

test_that("description generated correctly", {
  roc <- rd_roclet()
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @importFrom magrittr %>%
    #' @export
    magrittr::`%>%`
    "
  )[[1]]

  expect_null(out$get_section("description"))
})

test_that("only non-infix reexported functions get ()", {
  skip_on_cran()

  expect_equal(
    reexport_link("testthat", "test_that"),
    "\\code{\\link[testthat:test_that]{test_that()}}"
  )
  expect_equal(
    reexport_link("testthat", "CheckReporter"),
    "\\code{\\link[testthat:CheckReporter]{CheckReporter}}"
  )
  expect_equal(
    reexport_link("pkg", "%op%"),
    "\\code{\\link[pkg:\\%op\\%]{\\%op\\%}}"
  )
})
