context("block parsers")

block_registry <- function() {
  block_test <- function(x) {
    x$val <- toupper(x$val)
    x
  }

  list(
    title = block_test,
    description = block_test,
    details = block_test
  )
}

check <- function(out) {
  expect_equal(
    get_tag(out, "title"),
    roxy_field_simple("title", "TITLE")
  )
  expect_equal(
    get_tag(out, "description"),
    roxy_field_simple("description", "DESCRIPTION")
  )
  expect_equal(
    get_tag(out, "details"),
    roxy_field_simple("details", "DETAILS")
  )
}


test_that("block parsers are called for tags from intro", {

  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description
    #'
    #' Details
    f <- function(foo) 'foo'
  ", registry = block_registry())[[1]]

  check(out)
})


test_that("block parsers from intro & @title", {
  out <- roc_proc_text(rd_roclet(), "
    #' Description
    #'
    #' Details
    #'
    #' @title Title
    f <- function(foo) 'foo'
  ", registry = block_registry())[[1]]

  check(out)
})


test_that("block parsers from intro & @description", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Details
    #'
    #' @description Description
    f <- function(foo) 'foo'
  ", registry = block_registry())[[1]]

  check(out)
})


test_that("block parsers from intro & @details", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description
    #'
    #' @details Details
    f <- function(foo) 'foo'
  ", registry = block_registry())[[1]]

  check(out)
})
