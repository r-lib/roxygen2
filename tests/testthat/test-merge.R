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

test_that("@section-s with identical titles are merged", {
  out <- roc_proc_text(roc, "
    #' Foo
    #'
    #' @section Haz dox: Here.
    #'
    #' @section TL: DR.
    foo <- function(x = '%') x

    #' @rdname foo
    #' @section RT: FM.
    #' @section Haz dox:
    #'   Got news.
    bar <- function(y = '%') y
  ")[[1]]

  expect_equal(
    get_tag(out, "section")$values,
    list(structure(list(name = "Haz dox", content = " Here.\n\n\n\n\n\n\n  Got news.")),
         structure(list(name = "TL", content = "\n\n DR.\n\n")),
         structure(list(name = "RT", content = "\n\n FM.\n\n"))))
})
