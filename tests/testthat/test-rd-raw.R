context("Rd: raw output")

test_that("rawRd inserted unchanged", {
  out <- roc_proc_text(rd_roclet(), "
    #' @rawRd #this is a comment
    #' @name a
    #' @title a
    NULL")[[1]]

  lines <- strsplit(format(out), "\n")[[1]]
  expect_equal(lines[[9]], "#this is a comment")
})

test_that("evalRd must be valid code", {
  expect_warning(
    roc_proc_text(rd_roclet(), "
      #' @evalRd a +
      #' @name a
      #' @title a
      NULL"),
    "code failed to parse"
  )
})

test_that("error-ful evalRd generates warning", {
  expect_warning(
    roc_proc_text(rd_roclet(), "
      #' @evalRd stop('!')
      #' @name a
      #' @title a
      NULL"),
    "@evalRd failed with error"
  )
})

test_that("evalRd inserted unchanged", {
  out <- roc_proc_text(rd_roclet(), "
    z <- 10
    #' @evalRd as.character(z * 2)
    #' @name a
    #' @title a
    NULL")[[1]]

  args <- get_tag(out, "rawRd")$values
  expect_equal(args, "20")
})
