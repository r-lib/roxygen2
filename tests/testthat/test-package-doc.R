context("Package documentation")

test_that("can create package documentation", {
  with_mock(
    `roxygen2::read.description` = function(...)
      list(Package = "roxygen_devtest",
           Title = "Package Title",
           Description = "Package description."),
    out <- roc_proc_text(rd_roclet(), "
    #' @details Details.
    '_PACKAGE'")[[1]]
  )
  expect_equal(get_tag(out, "name")$values, "roxygen_devtest-package")
  expect_equal(get_tag(out, "alias")$values, c("roxygen_devtest",
                                               "roxygen_devtest-package"))
  expect_equal(get_tag(out, "title")$values, "Package Title")
  expect_equal(get_tag(out, "description")$values, "Package description.")
  expect_equal(get_tag(out, "docType")$values, "package")
  expect_equal(get_tag(out, "details")$values, "Details.")
})
