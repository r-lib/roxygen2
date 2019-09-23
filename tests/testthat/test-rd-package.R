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
  expect_equal(out$get_value("name"), "roxygen_devtest-package")
  expect_equal(out$get_value("alias"), c("roxygen_devtest", "roxygen_devtest-package"))
  expect_equal(out$get_value("title"), "roxygen_devtest: Package Title")
  expect_equal(out$get_value("description"), "Package description.")
  expect_equal(out$get_value("docType"), "package")
  expect_equal(out$get_value("details"), "Details.")
})

test_that("Can read UTF-8 DESCRIPTIONS", {
  expect_equal(read.description("testNonASCII/DESCRIPTION")$Author, "Shr\U00EBktan <shrektan@126.com>")
})
