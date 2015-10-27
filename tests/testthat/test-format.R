context("Format")

test_that("format defaults for list", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    x <- list(a = 1, b = 2)")[[1]]

  expect_equal(get_tag(out, "format")$values,
   rd("An object of class \\code{list} of length 2.")
  )
})

test_that("format defaults for matrices", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    x <- diag(10)")[[1]]

  expect_equal(get_tag(out, "format")$values,
               rd("An object of class \\code{matrix} with 10 rows and 10 columns.")
  )
})

test_that("format defaults for data frames", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    x <- data.frame(a = 1, b = 2)")[[1]]

  expect_equal(get_tag(out, "format")$values,
               rd("An object of class \\code{data.frame} with 1 rows and 2 columns.")
  )
})

test_that("format defaults for arrays", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    x <- array(1:27, dim = c(3, 3, 3))")[[1]]

  expect_equal(get_tag(out, "format")$values,
               rd("An object of class \\code{array} of dimension 3 x 3 x 3.")
  )
})

test_that("format defaults for xtabs", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    x <- xtabs(~cyl+am+gear, mtcars)")[[1]]

  expect_equal(get_tag(out, "format")$values,
               rd("An object of class \\code{xtabs} (inherits from \\code{table}) of dimension 3 x 2 x 3.")
  )
})

test_that("@format overrides defaults", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' @format abc
    #'
    x <- list(a = 1, b = 2)")[[1]]

  expect_equal(get_tag(out, "format")$values, "abc")
})

test_that("@format NULL suppresses default usage", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' @format NULL
    #'
    x <- list(a = 1, b = 2)")[[1]]

  expect_equal(get_tag(out, "format")$values, NULL)
})


test_that("@format not escaped", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #' @format %
    #'
    x <- list(a = 1, b = 2)")[[1]]

  expect_equal(get_tag(out, "format")$values, "%")
  expect_equal(format(get_tag(out, "format")), "\\format{%}\n")
})
