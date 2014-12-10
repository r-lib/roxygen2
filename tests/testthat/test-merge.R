context("Merging")
roc <- rd_roclet()

test_that("@description and @details are merged", {
  out <- roc_proc_text(roc, "
    #' Foo
    #'
    #' This.
    #'
    #' OBTW.
    foo <- function(x = '%') x

    #' @rdname foo
    #' @description And that.
    #' @details ORLY?
    bar <- function(y = '%') y
  ")[[1]]

  expect_equal(get_tag(out, "description")$values, c("This.", "And that."))
  expect_equal(get_tag(out, "details")$values, c("OBTW.", "ORLY?"))
})
