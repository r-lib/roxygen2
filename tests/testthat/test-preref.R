context("preref parsers")

set_preref <- function() {
  register.preref.parsers(
    preref_test,
    "title",
    "description",
    "details"
  )
}

restore_preref <- function() {
  register.preref.parsers(
    parse.value,
    "title",
    "description",
    "details"
  )
}

check <- function(out) {
  expect_equal(
    get_tag(out, "title"),
    structure(
      list(tag = "title", values = "TITLE"),
      class = c("title_tag", "rd_tag")
    )
  )
  expect_equal(
    get_tag(out, "description"),
    structure(
      list(tag = "description", values = "DESCRIPTION"),
      class = c("description_tag", "rd_tag")
    )
  )
  expect_equal(
    get_tag(out, "details"),
    structure(
      list(tag = "details", values = "DETAILS"),
      class = c("details_tag", "rd_tag")
    )
  )
}

preref_test <- function(key, rest) toupper(rest)

test_that("preref parsers are called for tags from intro", {

  on.exit(restore_preref())
  set_preref()

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


test_that("preref parsers from intro & @title", {

  on.exit(restore_preref())
  set_preref()

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


test_that("preref parsers from intro & @description", {

  on.exit(restore_preref())
  set_preref()

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


test_that("preref parsers from intro & @details", {

  on.exit(restore_preref())
  set_preref()

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
