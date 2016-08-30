context("Raw output")

test_that("rawRd inserted unchanged", {
  out <- roc_proc_text(rd_roclet(), "
    #' @rawRd #this is a comment
    #' @name a
    NULL")[[1]]

  lines <- strsplit(format(out), "\n")[[1]]
  expect_equal(lines[[5]], "#this is a comment")
})

test_that("evalRd must be valid code", {
  expect_warning(
    roc_proc_text(rd_roclet(), "
      #' @evalRd a +
      #' @name a
      NULL"),
    "code failed to parse"
  )
})

test_that("rawRd inserted unchanged", {
  out <- roc_proc_text(rd_roclet(), "
    z <- 10
    #' @evalRd z * 2
    #' @name a
    NULL")[[1]]

  args <- get_tag(out, "rawRd")$values
  expect_equal(args, "20")
})

test_that("rawNamespace must be valid code", {
  expect_warning(
    roc_proc_text(namespace_roclet(), "
      #' @rawNamespace if() {
      #' @name a
      NULL"),
    "code failed to parse"
  )
})

test_that("rawNamespace inserted unchanged", {
  out <- roc_proc_text(namespace_roclet(), "
    #' @rawNamespace xyz
    #'   abc
    NULL")

  expect_equal(out, "xyz\n  abc")
})
