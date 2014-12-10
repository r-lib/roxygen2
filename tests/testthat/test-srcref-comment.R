context("srcref")
roc <- rd_roclet()

test_that("Source reference is included as comment", {
  out <- roc_proc_text(roc, "
    #' @name a
    #' @docType package
    NULL")

  backref <- format(get_tag(out[[1]], "backref"))
  expect_match(backref, "^% Please edit documentation in ")
})
