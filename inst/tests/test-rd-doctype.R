context("Rd - docTypes")
roc <- rd_roclet()

# Package --------------------------------------------------------------------

test_that("@docType package automatically adds package alias when needed", {
  out <- roc_proc_text(roc, "
    #' @name a
    #' @docType package
    NULL

    #' @name a-package
    #' @docType package
    NULL")
  
  alias_1 <- get_tag(out[[1]], "alias")$values
  expect_equal(alias_1, c("a", "a-package"))
  
  alias_2 <- get_tag(out[[2]], "alias")$values
  expect_equal(alias_2, c("a-package"))
})


# Data --------------------------------------------------------------------

test_that("@docType data automatically adds sensible defaults", {
  out <- roc_proc_text(roc, "
    #' Title.
    #'
    #' @docType data
    a <- data.frame(a = 1:10)")[[1]]
  
  expect_equal(get_tag(out, "usage")$values, "a")
  expect_equal(get_tag(out, "keyword")$values, "datasets")
  expect_equal(is.null(get_tag(out, "format")$values), FALSE)
})

test_that("@docType data automatically added to data objects", {
  out <- roc_proc_text(roc, "
    #' Title.
    a <- data.frame(a = 1:10)")[[1]]
  
  expect_equal(get_tag(out, "docType")$values, "data")  
})

# Reference classes ----------------------------------------------------------

test_that("@docType data not automatically added to reference classes", {
  out <- roc_proc_text(roc, "
    #' Title.
    a <- setRefClass('a')")[[1]]
  
  expect_equal(get_tag(out, "docType")$values, NULL)  
})