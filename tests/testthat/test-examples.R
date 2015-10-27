context("Examples")

test_that("@example loads from specified files", {
  out <- roc_proc_text(rd_roclet(), "
    #' @name a
    #'
    #' @example Rd-example-1.R
    #'
    #' @example Rd-example-2.R
    NULL")[[1]]

  examples <- get_tag(out, "examples")$values
  expect_match(examples, fixed("example <- 'example1'"), all = FALSE)
  expect_match(examples, fixed("example <- 'example2'"), all = FALSE)
})

test_that("@examples captures examples", {
  out <- roc_proc_text(rd_roclet(), "
    #' @name a
    #'
    #' @examples a <- 2
    #'
    NULL")[[1]]

  examples <- get_tag(out, "examples")$values
  expect_match(examples, fixed("a <- 2"), all = FALSE)
})

test_that("@examples and @example combine", {
  out <- roc_proc_text(rd_roclet(), "
    #' @name a
    #' @example Rd-example-1.R
    #' @examples a <- 2
    NULL")[[1]]

  examples <- get_tag(out, "examples")$values
  expect_match(examples, fixed("example <- 'example1'"), all = FALSE)
  expect_match(examples, fixed("a <- 2"), all = FALSE)
})

test_that("@example does not introduce extra empty lines", {
  out <- roc_proc_text(rd_roclet(), "
    #' @name a
    #' @example Rd-example-3.R
    NULL")[[1]]

  examples <- get_tag(out, "examples")$values
  expect_identical(length(examples), 2L)
})

test_that("indentation in examples preserved", {
  out <- roc_proc_text(rd_roclet(), "
    #' @name a
    #' @examples a <-
    #'     2
    NULL")[[1]]

  examples <- get_tag(out, "examples")$values
  expect_match(examples, fixed("a <-\n    2"), all = FALSE)
})

test_that("% and \\ in @example escaped", {
  out <- roc_proc_text(rd_roclet(), "
    #' @name a
    #' @example Rd-example-4.R
    NULL")[[1]]

  examples <- get_tag(out, "examples")$values
  expect_equal(examples, rd("x \\%*\\% y # \\\\x"))
})

test_that("\\dontrun in @example unescaped", {
  out <- roc_proc_text(rd_roclet(), "
    #' @name a
    #' @example Rd-example-5.R
    NULL")[[1]]

  examples <- get_tag(out, "examples")$values
  expect_equal(examples, rd("\\dontrun{x <- 1}"))
})

test_that("% in @examples escaped before matching braces test (#213)", {
  out <- roc_proc_text(rd_roclet(), "
    #' @name a
    #' @examples
    #' {a %% b}
    NULL")[[1]]

  examples <- get_tag(out, "examples")$values
  expect_equal(examples, rd("{a \\%\\% b}"))
})
