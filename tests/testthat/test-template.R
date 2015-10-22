context("Template")

test_that("template_find finds files with .r and .R extension, and fails to find missing files", {
  my.tempdir <- "."
  my.mandir <- file.path(my.tempdir, "man-roxygen")
  my.ucase <- file.path(my.mandir, "UCase.R")
  my.regex <- file.path(my.mandir, "reg.ex.R")
  my.lcase <- file.path(my.mandir, "lcase.r")

  expect_equal(template_find(my.tempdir, "UCase"), my.ucase)
  expect_error(template_find(my.tempdir, "Case"))
  expect_error(template_find(my.tempdir, "UCas"))
  expect_equal(template_find(my.tempdir, "reg.ex"), my.regex)
  expect_error(template_find(my.tempdir, "reggex"))
  expect_error(template_find(my.tempdir, "nada"))

  # On case-insentive file systems, will find upper case version first
  expect_equal(tolower(template_find(my.tempdir, "lcase")), tolower(my.lcase))
})

test_that("templates replace variables with their values", {
  out <- roc_proc_text(rd_roclet(), "
    #' @template values
    #' @templateVar x a
    #' @templateVar y b
    #' @templateVar z c
    x <- 10")[[1]]

  expect_equal(get_tag(out, "title")$values, "a")
  expect_equal(get_tag(out, "param")$values, c(b = "c"))
})

test_that("allow empty line after @template", {
  out <- roc_proc_text(rd_roclet(), "
    #' @template values
    #'
    #' @templateVar x a
    #' @templateVar y b
    #' @templateVar z c
    x <- 10")[[1]]

  expect_equal(get_tag(out, "title")$values, "a")
})
