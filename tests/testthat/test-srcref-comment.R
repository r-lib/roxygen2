context("srcref")
test_that("Source reference is included as comment", {
  out <- roc_proc_text(rd_roclet(), "
    #' @name a
    #' @docType package
    NULL")

  backref <- format(get_tag(out[[1]], "backref"))
  expect_match(backref, "^% Please edit documentation in ")
})

test_that("Explicit @backref is included as comment", {
  out <- roc_proc_text(rd_roclet(), "
    #' @name a
    #' @backref back/ref.file
    #' @backref root.file
    #' @docType package
    NULL")

  backref <- format(get_tag(out[[1]], "backref"))
  expect_match(backref, "^% Please edit documentation in back/ref[.]file, [.]/root[.]file")
})
