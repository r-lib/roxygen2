context("Format")
roc <- rd_roclet()

test_that("@format overrides defaults", {
  out <- roc_proc_text(roc, "
    #' Title
    #' @format abc
    x <- list(a = 1, b = 2)")[[1]]

  expect_equal(get_tag(out, "format")$values, "abc")
})

test_that("@format NULL suppresses default usage", {
  out <- roc_proc_text(roc, "
    #' Title
    #' @format NULL
    x <- list(a = 1, b = 2)")[[1]]

  expect_equal(get_tag(out, "format")$values, NULL)
})


test_that("@format not escaped", {
  out <- roc_proc_text(roc, "
    #' Title
    #' @format %
    x <- list(a = 1, b = 2)")[[1]]

  expect_equal(get_tag(out, "format")$values, "%")
  expect_equal(format(get_tag(out, "format")), "\\format{%}\n")
})
