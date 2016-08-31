context("block parsers")

set_block <- function() {
  register_tags(
    title = block_test,
    description = block_test,
    details = block_test
  )
}

restore_block <- function() {
  register_tags(
    title = tag_value,
    description = tag_value,
    details = tag_value
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

block_test <- function(x) {
  x$val <- toupper(x$val)
  x
}

test_that("block parsers are called for tags from intro", {

  on.exit(restore_block())
  set_block()

  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description
    #'
    #' Details
    f <- function(foo) 'foo'
  ")[[1]]

  check(out)
})


test_that("block parsers from intro & @title", {

  on.exit(restore_block())
  set_block()

  out <- roc_proc_text(rd_roclet(), "
    #' Description
    #'
    #' Details
    #'
    #' @title Title
    f <- function(foo) 'foo'
  ")[[1]]

  check(out)
})


test_that("block parsers from intro & @description", {

  on.exit(restore_block())
  set_block()

  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Details
    #'
    #' @description Description
    f <- function(foo) 'foo'
  ")[[1]]

  check(out)
})


test_that("block parsers from intro & @details", {

  on.exit(restore_block())
  set_block()

  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description
    #'
    #' @details Details
    f <- function(foo) 'foo'
  ")[[1]]

  check(out)
})
