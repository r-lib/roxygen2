context("Format")
roc <- rd_roclet()
  
test_that("format defaults to output from str", {
  out <- roc_proc_text(roc, "
    #' Title
    x <- list(a = 1, b = 2)")[[1]]
  
  expect_equal(get_tag(out, "format")$values, 
   rd("\\preformatted{\nList of 2\n $ a: num 1\n $ b: num 2\n}")
  )
})

test_that("@format overrides defaults", {
  out <- roc_proc_text(roc, "
    #' Title
    #' @format abc
    x <- list(a = 1, b = 2)")[[1]]
  
  expect_equal(get_tag(out, "format")$values, "abc")
})

test_that("@format not escaped", {
  out <- roc_proc_text(roc, "
    #' Title
    #' @format %
    x <- list(a = 1, b = 2)")[[1]]
  
  expect_equal(get_tag(out, "format")$values, "%")
  expect_equal(format(get_tag(out, "format")), "\\format{%}\n")
})