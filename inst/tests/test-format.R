context("Format")
roc <- rd_roclet()
  
test_that("format defaults to output from str", {
  out <- roc_proc_text(roc, "
    #' Title
    x <- list(a = 1, b = 2)")[[1]]
  
  expect_equal(get_tag(out, "format")$values, 
    "List of 2\n $ a: num 1\n $ b: num 2"
  )
})

test_that("@format overrides defaults", {
  out <- roc_proc_text(roc, "
    #' Title
    #' @format abc
    x <- list(a = 1, b = 2)")[[1]]
  
  expect_equal(get_tag(out, "format")$values, "abc")
})