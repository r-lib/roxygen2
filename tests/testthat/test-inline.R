context("Inline")

test_that("Inline comments are supported", {
  out <- roc_proc_text(rd_roclet(), "
    #' Description
    a <- function(x) {
      #' @param x an integer
      stopifnot(is.integer(x))
    }")[[1]]
  expect_equal(get_tag(out, "param")$values, c(x = "an integer"))
})

test_that("Inline comments just before the closing brace are allowed", {
  out <- roc_proc_text(rd_roclet(), "
    #' Description
    a <- function(x) {
      #' @param x an integer
      stopifnot(is.integer(x))

      #' @seealso somewhere
    }")[[1]]
  expect_equal(get_tag(out, "seealso")$value, "somewhere")
})

test_that("Inline comments do not extend past the closing brace", {
  out <- roc_proc_text(rd_roclet(), "
    #' Description
    a <- function(x) {
      #' @param x an integer
      stopifnot(is.integer(x))
    }; #' @seealso somewhere")[[1]]
  expect_null(get_tag(out, "seealso"))
})
