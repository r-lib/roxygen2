context("Introduction")

test_that("title and description taken from first line if only one", {
  out <- roc_proc_text(rd_roclet(), "
    #' description
    #' @name a
    NULL")[[1]]
  expect_equal(get_tag(out, "description")$values, "description")
  expect_equal(get_tag(out, "title")$values, "description")
})

test_that("title, description and details extracted correctly", {
  out <- roc_proc_text(rd_roclet(), "
    #' title
    #'
    #' description
    #'
    #' details
    #' @name a
    NULL")[[1]]
  expect_equal(get_tag(out, "description")$values, "description")
  expect_equal(get_tag(out, "details")$values, "details")
})

test_that("title taken from first paragraph", {
  out <- roc_proc_text(rd_roclet(), "
    #' Description with sentence.
    #'
    #' That continueth.
    #' @name a
    NULL")[[1]]
  expect_equal(get_tag(out, "title")$values, "Description with sentence.")
  expect_equal(get_tag(out, "description")$values,
    "That continueth.")
})

test_that("@title overrides default title", {
  out <- roc_proc_text(rd_roclet(), "
    #' Would be title
    #' @title Overridden title
    #' @name a
    NULL")[[1]]
  expect_equal(get_tag(out, "title")$values, "Overridden title")
  expect_equal(get_tag(out, "description")$values, "Would be title")
})

test_that("docs parsed correctly if no blank text", {
  out <- roc_proc_text(rd_roclet(), "
    #' @title My title
    #' @description My description
    #' @param x value
    a <- function(x) {}")[[1]]

  expect_equal(get_tag(out, "title")$values, "My title")
  expect_equal(get_tag(out, "description")$values, "My description")
})

test_that("question mark ends sentence", {
  out <- roc_proc_text(rd_roclet(), "
    #' Is a number odd?
    is.odd <- function(a) {}")[[1]]
  expect_equal(get_tag(out, "title")$values, "Is a number odd?")

})

test_that("no ending punctuation does not produce ellipsis", {
  out <- roc_proc_text(rd_roclet(), "
    #' Whether a number is odd
    is.odd <- function(a) {}")[[1]]
  expect_equal(get_tag(out, "title")$values, "Whether a number is odd")
})

test_that("details are merged if needed", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description
    #'
    #' Details1
    #'
    #' Details2
    #'
    #' @details Details3
    #'
    #' Details4
    foo <- function(x) {}")[[1]]

  expect_equal(get_tag(out, "details")$values,
               "Details1\n\nDetails2\n\nDetails3\n\nDetails4")
})

test_that("whitespace is not detected as details", {
  expect_silent(
    out <- roc_proc_text(
      rd_roclet(), "
        #' Title
        #'
        #'
        #' Description
        #'
        #'
        #'
        foo <- function(x) {}"
    )[[1]]
  )

  expect_null(get_tag(out, "details"))
})
