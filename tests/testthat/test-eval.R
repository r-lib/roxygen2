context("eval")

test_that("evaluation occurs during parsing", {
  out <- roc_proc_text(rd_roclet(), "
    foo <- function() c('@name a', '@title a')
    #' @eval foo()
    NULL")[[1]]

  expect_equal(out$get_field("title")$values, "a")
  expect_equal(out$filename, "a.Rd")
})

test_that("errors are propagated", {
  expect_warning(
    roc_proc_text(rd_roclet(), "
      foo <- function() stop('Uhoh')
      #' @eval foo()
      NULL"
    ),
    "@eval failed with error"
  )
})

test_that("must return non-NA string", {
  expect_warning(
    roc_proc_text(rd_roclet(), "
      foo <- function() NA
      #' @eval foo()
      NULL"
    ),
    "@eval did not evaluate to a string"
  )

  expect_warning(
    roc_proc_text(rd_roclet(), "
      foo <- function() NA_character_
      #' @eval foo()
      NULL"
    ),
    "@eval result contained NA"
  )
})


test_that("also works with namespace roclet", {
  out <- roc_proc_text(namespace_roclet(), "
    foo <- function() '@export a'
    #' @eval foo()
    #' @name a
    #' @title a
    NULL")

  expect_equal(out, "export(a)")
})
