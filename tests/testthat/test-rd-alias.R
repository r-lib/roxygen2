context("Rd: alias")

test_that("aliases split into pieces", {
  out <- roc_proc_text(rd_roclet(), "
    #' @aliases a b
    #' @title a
    #' @name a
    NULL")[[1]]

  expect_match(out$get_value("alias"), fixed("a"), all = FALSE)
  expect_match(out$get_value("alias"), fixed("b"), all = FALSE)
})

test_that("aliases escaped, not quoted", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' @name %a%
    #' @aliases a
    #' @title a
    NULL")[[1]]
  expect_equal(out1$get_rd("alias"), c("\\alias{\\%a\\%}", "\\alias{a}"))

  out2 <- roc_proc_text(rd_roclet(), "
    #' @name a
    #' @aliases %a%
    #' @title a
    NULL")[[1]]
  expect_equal(out2$get_rd("alias"), c("\\alias{a}", "\\alias{\\%a\\%}"))
})

test_that("can use NULL to suppress default aliases", {
  out <- roc_proc_text(rd_roclet(), "
    #' @aliases NULL
    #' @title a
    #' @name a
    NULL")[[1]]

  expect_equal(out$get_value("alias"), character())
})

test_that("aliases get deduplicated", {
  out <- roc_proc_text(rd_roclet(), "
    #' @aliases a b a
    #' @title a
    #' @name a
    NULL")[[1]]

  expect_equal(out$get_rd("alias"), c("\\alias{a}", "\\alias{b}"))
})

test_that("aliases get deduplicated with defaults suppressed", {
  out <- roc_proc_text(rd_roclet(), "
    #' @aliases NULL b c b
    #' @title a
    #' @name a
    NULL")[[1]]

  expect_equal(out$get_rd("alias"), c("\\alias{b}", "\\alias{c}"))
})

test_that("refclass with assignment gets both aliases", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    B3 <- setRefClass('B3')
  ")[[1]]

  expect_equal(out$get_value("alias"), c("B3-class", "B3"))
})


test_that("refclass gets -class alias", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    setRefClass('B2')
  ")[[1]]

  expect_equal(out$get_value("alias"), "B2-class")
})
