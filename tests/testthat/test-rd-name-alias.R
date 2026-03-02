# name --------------------------------------------------------------------

test_that("name captured from assignment", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title.
    a <- function() {}
  "
  )[[1]]

  expect_equal(out$get_value("name"), "a")
  expect_equal(out$get_value("alias"), "a")
  expect_equal(out$get_value("title"), "Title.")
})

test_that("name also captured from assignment by =", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title.
    a = function() {}
  "
  )[[1]]

  expect_equal(out$get_value("name"), "a")
  expect_equal(out$get_value("alias"), "a")
  expect_equal(out$get_value("title"), "Title.")
})


test_that("`$` not to be parsed as assignee in foo$bar(a = 1)", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' foo object
    foo <- list(bar = function(a) a)
    foo$bar(a = 1)
  "
  )[[1]]

  expect_equal(out$get_value("name"), "foo")
})

test_that("names escaped, not quoted", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    '%a%' <- function(x, y) x + y
  "
  )[[1]]
  expect_equal(out$get_rd("name"), "\\name{\\%a\\%}")
})

test_that("quoted names captured from assignment", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title.
    \"myfunction\" <- function(...) {}
  "
  )[[1]]

  expect_equal(out$get_value("name"), "myfunction")
  expect_equal(out$get_value("alias"), "myfunction")

  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title.
    `myfunction` <- function(...) {}
  "
  )[[1]]
  expect_equal(out$get_value("name"), "myfunction")
  expect_equal(out$get_value("alias"), "myfunction")

  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title.
    \"my function\" <- function(...) {}
  "
  )[[1]]

  expect_equal(out$get_value("name"), "my function")
  expect_equal(out$get_value("alias"), "my function")
})

test_that("@name overides default", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A
    #' @name b
    a <- function() {}
  "
  )[[1]]

  expect_equal(out$get_value("name"), "b")
  expect_setequal(out$get_value("alias"), c("a", "b"))
})


# alias -------------------------------------------------------------------

test_that("aliases split into pieces", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @aliases a b
    #' @title a
    #' @name a
    NULL"
  )[[1]]

  expect_match(out$get_value("alias"), fixed("a"), all = FALSE)
  expect_match(out$get_value("alias"), fixed("b"), all = FALSE)
})

test_that("aliases escaped, not quoted", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' @name %a%
    #' @aliases a
    #' @title a
    NULL"
  )[[1]]
  expect_equal(out1$get_rd("alias"), c("\\alias{\\%a\\%}", "\\alias{a}"))

  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' @name a
    #' @aliases %a%
    #' @title a
    NULL"
  )[[1]]
  expect_equal(out2$get_rd("alias"), c("\\alias{a}", "\\alias{\\%a\\%}"))
})

test_that("can use NULL to suppress default aliases", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @aliases NULL
    #' @title a
    #' @name a
    NULL"
  )[[1]]

  expect_equal(out$get_value("alias"), character())
})

test_that("aliases get deduplicated", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @aliases a b a
    #' @title a
    #' @name a
    NULL"
  )[[1]]

  expect_equal(out$get_rd("alias"), c("\\alias{a}", "\\alias{b}"))
})

test_that("aliases get deduplicated with defaults suppressed", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @aliases NULL b c b
    #' @title a
    #' @name a
    NULL"
  )[[1]]

  expect_equal(out$get_rd("alias"), c("\\alias{b}", "\\alias{c}"))
})

test_that("refclass with assignment gets both aliases", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    B3 <- setRefClass('B3')
  "
  )[[1]]

  expect_equal(out$get_value("alias"), c("B3-class", "B3"))
})


test_that("refclass gets -class alias", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    setRefClass('B2')
  "
  )[[1]]

  expect_equal(out$get_value("alias"), "B2-class")
})
