context("srcref")
roc <- rd_roclet()

test_that("Source reference is included as comment", {
  out <- roc_proc_text(roc, "
    #' @name a
    #' @docType package
    NULL")

  srcref <- format(get_tag(out[[1]], "srcref"))
  expect_match(srcref, "^% Please edit documentation in ")
})
