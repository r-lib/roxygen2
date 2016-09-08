context("Rd: inherit")

# tag parsing -------------------------------------------------------------

test_that("warns on unknown inherit type", {
  expect_warning(
    parse_text("
      #' @inherit fun blah
      NULL
    "),
    "Unknown inherit type: blah"
  )
})

test_that("no options gives default values", {
  parsed <- parse_text("
    #' @inherit fun
    NULL
  ")
  block <- parsed$blocks[[1]]

  expect_equal(block$inherit$fields, c("params", "slots", "return"))
})

test_that("some options overrides defaults", {
  parsed <- parse_text("
    #' @inherit fun return
    NULL
  ")
  block <- parsed$blocks[[1]]

  expect_equal(block$inherit$fields, "return")
})


# Inherit return values ---------------------------------------------------

test_that("can inherit return values from roxygen topic", {
  out <- roc_proc_text(rd_roclet(), "
    #' A.
    #'
    #' @return ABC
    a <- function(x) {}


    #' B
    #'
    #' @inherit a
    b <- function(y) {}
  ")[[2]]

  expect_equal(out$get_field("value")$values, "ABC")
})


test_that("takes value from first with return", {
  out <- roc_proc_text(rd_roclet(), "
    #' A1
    #' @return A
    a1 <- function(x) {}

    #' A2
    a2 <- function() {}

    #' B
    #' @return B
    b <- function(x) {}

    #' C
    #' @inherit a2
    #' @inherit b
    #' @inherit a1
    c <- function(y) {}
  ")[[3]]

  expect_equal(out$get_field("value")$values, "B")
})

test_that("can inherit return value from external function", {
  out <- roc_proc_text(rd_roclet(), "
    #' A1
    #' @inherit base::mean
    a1 <- function(x) {}
  ")[[1]]

  expect_match(out$get_field("value")$values, "before the mean is computed.$")
  expect_match(out$get_field("value")$values, "^If \\\\code")
})
