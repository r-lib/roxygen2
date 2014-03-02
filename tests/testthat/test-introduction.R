context("Introduction")
roc <- rd_roclet()

test_that("title and description taken from first line if only one", {
  out <- roc_proc_text(roc, "
    #' description
    #' @name a
    NULL")[[1]]
  expect_equal(get_tag(out, "description")$values, "description")
  expect_equal(get_tag(out, "title")$values, "description")
})

test_that("title, description and details extracted correctly", {
  out <- roc_proc_text(roc, "
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
  out <- roc_proc_text(roc, "
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
  out <- roc_proc_text(roc, "
    #' Would be title
    #' @title Overridden title
    #' @name a
    NULL")[[1]]
  expect_equal(get_tag(out, "title")$values, "Overridden title")
  expect_equal(get_tag(out, "description")$values, "Would be title")
})

test_that("docs parsed correctly if no blank text", {
  out <- roc_proc_text(roc, "
    #' @title My title
    #' @description My description
    #' @param x value
    a <- function(x) {}")[[1]]

  expect_equal(get_tag(out, "title")$values, "My title")
  expect_equal(get_tag(out, "description")$values, "My description")
})

test_that("question mark ends sentence", {
  out <- roc_proc_text(roc, "
    #' Is a number odd?
    is.odd <- function(a) {}")[[1]]
  expect_equal(get_tag(out, "title")$values, "Is a number odd?")

})

test_that("no ending punctuation does not produce ellipsis", {
  out <- roc_proc_text(roc, "
    #' Whether a number is odd
    is.odd <- function(a) {}")[[1]]
  expect_equal(get_tag(out, "title")$values, "Whether a number is odd")
})

