
# object ------------------------------------------------------------------

test_that("has thoughtful print method", {
  text <- "
    #' This is a title
    #'
    #' @param x,y A number
    #' @export
    f <- function(x, y) x + y
  "
  block <- parse_text(text)[[1]]
  verify_output(test_path("test-block-print.txt"), block)
})

# description block -------------------------------------------------------

test_that("title and description taken from first line if only one", {
  out <- roc_proc_text(rd_roclet(), "
    #' title
    #' @name a
    NULL")[[1]]
  expect_equal(get_tag(out, "description")$values, "title")
  expect_equal(get_tag(out, "title")$values, "title")
})

test_that("description taken from multiple titles if merged", {
  out <- roc_proc_text(rd_roclet(), "
    #' T1
    #' @name a
    NULL

    #' T2
    #' @name a
    NULL
    ")[[1]]
  expect_equal(get_tag(out, "title")$values, c("T1", "T2"))
  expect_equal(get_tag(out, "description")$values, c("T1", "T2"))
})

test_that("title, description and details extracted correctly", {
  out <- roc_proc_text(rd_roclet(), "
    #' title
    #'
    #' description
    #'
    #' details
    #' @name a
    NULL")[[1]]
  expect_equal(get_tag(out, "description")$values, "description")
  expect_equal(get_tag(out, "details")$values, "details")
})

test_that("title taken from first paragraph", {
  out <- roc_proc_text(rd_roclet(), "
    #' Description with sentence.
    #'
    #' That continueth.
    #' @name a
    NULL")[[1]]
  expect_equal(get_tag(out, "title")$values, "Description with sentence.")
  expect_equal(get_tag(out, "description")$values,
    "That continueth.")
})

test_that("@title overrides default title", {
  out <- roc_proc_text(rd_roclet(), "
    #' Would be title
    #' @title Overridden title
    #' @name a
    NULL")[[1]]
  expect_equal(get_tag(out, "title")$values, "Overridden title")
  expect_equal(get_tag(out, "description")$values, "Would be title")
})

test_that("docs parsed correctly if no blank text", {
  out <- roc_proc_text(rd_roclet(), "
    #' @title My title
    #' @description My description
    #' @param x value
    a <- function(x) {}")[[1]]

  expect_equal(get_tag(out, "title")$values, "My title")
  expect_equal(get_tag(out, "description")$values, "My description")
})

test_that("question mark ends sentence", {
  out <- roc_proc_text(rd_roclet(), "
    #' Is a number odd?
    is.odd <- function(a) {}")[[1]]
  expect_equal(get_tag(out, "title")$values, "Is a number odd?")

})

test_that("no ending punctuation does not produce ellipsis", {
  out <- roc_proc_text(rd_roclet(), "
    #' Whether a number is odd
    is.odd <- function(a) {}")[[1]]
  expect_equal(get_tag(out, "title")$values, "Whether a number is odd")
})

test_that("details are merged if needed", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description
    #'
    #' Details1
    #'
    #' Details2
    #'
    #' @details Details3
    #'
    #' Details4
    foo <- function(x) {}")[[1]]

  expect_equal(get_tag(out, "details")$values,
               "Details1\n\nDetails2\n\nDetails3\n\nDetails4")
})

test_that("whitespace is not detected as details", {
  expect_silent(
    out <- roc_proc_text(
      rd_roclet(), "
        #' Title
        #'
        #'
        #' Description
        #'
        #'
        #'
        foo <- function(x) {}"
    )[[1]]
  )

  expect_null(get_tag(out, "details"))
})


test_that("@description and @details are merged", {
  out <- roc_proc_text(rd_roclet(), "
    #' Foo
    #'
    #' This.
    #'
    #' OBTW.
    foo <- function(x = '%') x

    #' @rdname foo
    #' @description And that.
    #' @details ORLY?
    bar <- function(y = '%') y
  ")[[1]]

  expect_equal(get_tag(out, "description")$values, c("This.", "And that."))
  expect_equal(get_tag(out, "details")$values, c("OBTW.", "ORLY?"))
})

test_that("empty description block is silently removed", {
  expect_warning(
    roc_proc_text(rd_roclet(), "
      #'
      #'
      f <- function() {}
      "
    ),
    NA
  )
})

test_that("description block preserves whitespace", {
  out <- parse_text("
    #' Title
    #'
    #' Line 1
    #'   Line 2
    #'
    #' Line 1
    #'   Line 2
    f <- function() {}
    "
  )[[1]]

  expect_equal(block_get_tag_value(out, "description"), "Line 1\n  Line 2")
  expect_equal(block_get_tag_value(out, "details"), "Line 1\n  Line 2")
})


# evaluate ----------------------------------------------------------------

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
    "failed with error"
  )
})

test_that("must return non-NA string", {
  expect_warning(
    roc_proc_text(rd_roclet(), "
      foo <- function() NA
      #' @eval foo()
      NULL"
    ),
    "did not evaluate to a string"
  )

  expect_warning(
    roc_proc_text(rd_roclet(), "
      foo <- function() NA_character_
      #' @eval foo()
      NULL"
    ),
    "result contained NA"
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

