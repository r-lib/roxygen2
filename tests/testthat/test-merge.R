context("Merging")

test_that("@description and @details are merged", {
  out <- roc_proc_text(rd_roclet(), "
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
  out <- roc_proc_text(rd_roclet(), "
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
    list(structure(list(name = "Haz dox", content = " Here.\n\n\n  Got news.")),
         structure(list(name = "TL", content = " DR.")),
         structure(list(name = "RT", content = " FM."))))
})

test_that("@section-s with different titles are kept as they are", {
  out <- roc_proc_text(rd_roclet(), "
    #' Foo
    #'
    #' @section Haz dox: Here.
    #'
    #' @section TL: DR.
    foo <- function(x = '%') x

    #' @rdname foo
    #' @section OBTW:
    #'   Got news.
    bar <- function(y = '%') y

    #' @rdname foo
    #' @section MKAY: kthxbye
    baz <- function(y = '%') y
  ")[[1]]

  expect_equal(
    get_tag(out, "section")$values,
    list(structure(list(name = "Haz dox", content = " Here.")),
         structure(list(name = "TL", content = " DR.")),
         structure(list(name = "OBTW", content = "\n  Got news.")),
         structure(list(name = "MKAY", content = " kthxbye"))))
})
