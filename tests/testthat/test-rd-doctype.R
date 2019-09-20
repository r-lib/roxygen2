context("Rd: docType")

# Data --------------------------------------------------------------------

test_that("@docType data automatically adds sensible defaults", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    #'
    #' @docType data
    a <- data.frame(a = 1:10)")[[1]]

  expect_equal(get_tag(out, "usage")$values, rd("a"))
  expect_equal(get_tag(out, "keyword")$values, "datasets")
  expect_equal(is.null(get_tag(out, "format")$values), FALSE)
})

test_that("@docType data automatically added to data objects", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    a <- data.frame(a = 1:10)")[[1]]

  expect_equal(get_tag(out, "docType")$values, "data")
})

test_that("@docType data automatically added to data objects created elsewhere", {
  out <- roc_proc_text(rd_roclet(), "
    a <- data.frame(a = 1:10)
    #' Title.
    'a'")[[1]]

  expect_equal(get_tag(out, "docType")$values, "data")
  expect_equal(get_tag(out, "usage")$values, rd("a"))
  expect_equal(get_tag(out, "keyword")$values, "datasets")
})


# Reference classes ----------------------------------------------------------

test_that("@docType class automatically added to reference class objects", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    #'
    a <- setRefClass('a')")[[1]]

  expect_equal(get_tag(out, "class")$values, NULL)
})
