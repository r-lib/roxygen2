context("Rd: alias")

test_that("aliases split into pieces", {
  out <- roc_proc_text(rd_roclet(), "
    #' @aliases a b
    #' @title a
    #' @name a
    NULL")[[1]]

  expect_match(get_tag(out, "alias")$values, fixed("a"), all = FALSE)
  expect_match(get_tag(out, "alias")$values, fixed("b"), all = FALSE)
})

test_that("aliases escaped, not quoted", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' @name %a%
    #' @aliases a
    #' @title a
    NULL")[[1]]
  out2 <- roc_proc_text(rd_roclet(), "
    #' @name a
    #' @aliases %a%
    #' @title a
    NULL")[[1]]
  alias1 <- format(get_tag(out1, "alias"))
  alias2 <- format(get_tag(out2, "alias"))
  expect_equal(alias1, c("\\alias{\\%a\\%}", "\\alias{a}"))
  expect_equal(alias2, c("\\alias{a}", "\\alias{\\%a\\%}"))
})

test_that("can use NULL to suppress default aliases", {
  out <- roc_proc_text(rd_roclet(), "
    #' @aliases NULL
    #' @title a
    #' @name a
    NULL")[[1]]

  expect_equal(get_tag(out, "alias")$values, character())
})

test_that("aliases get deduplicated", {
  out <- roc_proc_text(rd_roclet(), "
    #' @aliases a b a
    #' @title a
    #' @name a
    NULL")[[1]]

  expect_equal(format(get_tag(out, "alias")), c("\\alias{a}", "\\alias{b}"))
})

test_that("aliases get deduplicated with defaults suppressed", {
  out <- roc_proc_text(rd_roclet(), "
    #' @aliases NULL b c b
    #' @title a
    #' @name a
    NULL")[[1]]

  expect_equal(format(get_tag(out, "alias")), c("\\alias{b}", "\\alias{c}"))
})

test_that("refclass with assignment gets both aliases", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    B3 <- setRefClass('B3')
  ")[[1]]

  expect_equal(get_tag(out, "alias")$values, c("B3-class", "B3"))
})


test_that("refclass gets -class alias", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    setRefClass('B2')
  ")[[1]]

  expect_equal(get_tag(out, "alias")$values, "B2-class")
})
