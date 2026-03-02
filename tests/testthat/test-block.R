# object ------------------------------------------------------------------

test_that("has thoughtful print method", {
  text <-
    "#' This is a title
    #'
    #' @param x,y A number
    #' @export
    f <- function(x, y) x + y
  "
  expect_snapshot(parse_text(text)[[1]])
})

# description block -------------------------------------------------------

test_that("title and description taken from first line if only one", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' title
    #' @name a
    NULL"
  )[[1]]
  expect_equal(out$get_value("description"), "title")
  expect_equal(out$get_value("title"), "title")
})

test_that("description taken from multiple titles if merged", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' T1
    #' @name a
    NULL

    #' T2
    #' @name a
    NULL
    "
  )[[1]]
  expect_equal(out$get_value("title"), c("T1", "T2"))
  expect_equal(out$get_value("description"), c("T1", "T2"))
})

test_that("title, description and details extracted correctly", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' title
    #'
    #' description
    #'
    #' details
    #' @name a
    NULL"
  )[[1]]
  expect_equal(out$get_value("description"), "description")
  expect_equal(out$get_value("details"), "details")
})

test_that("title taken from first paragraph", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Description with sentence.
    #'
    #' That continueth.
    #' @name a
    NULL"
  )[[1]]
  expect_equal(out$get_value("title"), "Description with sentence.")
  expect_equal(out$get_value("description"), "That continueth.")
})

test_that("@title overrides default title", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Would be title
    #' @title Overridden title
    #' @name a
    NULL"
  )[[1]]
  expect_equal(out$get_value("title"), "Overridden title")
  expect_equal(out$get_value("description"), "Would be title")
})

test_that("docs parsed correctly if no blank text", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @title My title
    #' @description My description
    #' @param x value
    a <- function(x) {}"
  )[[1]]

  expect_equal(out$get_value("title"), "My title")
  expect_equal(out$get_value("description"), "My description")
})

test_that("question mark ends sentence", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Is a number odd?
    is.odd <- function(a) {}"
  )[[1]]
  expect_equal(out$get_value("title"), "Is a number odd?")
})

test_that("no ending punctuation does not produce ellipsis", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Whether a number is odd
    is.odd <- function(a) {}"
  )[[1]]
  expect_equal(out$get_value("title"), "Whether a number is odd")
})

test_that("details are merged if needed", {
  out <- roc_proc_text(
    rd_roclet(),
    "
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
    foo <- function(x) {}"
  )[[1]]

  expect_equal(
    out$get_value("details"),
    "Details1\n\nDetails2\n\nDetails3\n\nDetails4"
  )
})

test_that("whitespace is not detected as details", {
  expect_silent(
    out <- roc_proc_text(
      rd_roclet(),
      "
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

  expect_null(out$get_value("details"))
})


test_that("@description and @details are merged", {
  out <- roc_proc_text(
    rd_roclet(),
    "
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
  "
  )[[1]]

  expect_equal(out$get_value("description"), c("This.", "And that."))
  expect_equal(out$get_value("details"), c("OBTW.", "ORLY?"))
})

test_that("empty description block is silently removed", {
  expect_warning(
    roc_proc_text(
      rd_roclet(),
      "
      #'
      #'
      f <- function() {}
      "
    ),
    NA
  )
})

test_that("description block preserves whitespace", {
  out <- parse_text(
    "
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


test_that("line numbers offset correctly", {
  out <- parse_text(
    "#' Title
    #'
    #' Line 3
    #' Line 4
    #' Line 5
    #'
    #' Line 7
    #' Line 8
    f <- function() {}
    "
  )[[1]]

  expect_equal(out$tags[[1]]$line, 1)
  expect_equal(out$tags[[2]]$line, 3)
  expect_equal(out$tags[[3]]$line, 7)
})

test_that("even with explicit title/description", {
  out <- parse_text(
    "#' Line 1
    #' Line 2
    #' Line 3
    #'
    #' Line 5
    #' Line 6
    #' @title This is a title
    f <- function() {}
    "
  )[[1]]

  expect_equal(out$tags[[1]]$line, 1)
  expect_equal(out$tags[[2]]$line, 5)
  expect_equal(out$tags[[3]]$line, 7)
})

# evaluate ----------------------------------------------------------------

test_that("evaluation occurs during parsing", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    foo <- function() c('@name a', '@title a')
    #' @eval foo()
    NULL"
  )[[1]]

  expect_equal(out$get_value("title"), "a")
  expect_equal(out$filename, "a.Rd")
})

test_that("errors are propagated", {
  block <- "
    foo <- function() stop('Uhoh')
    #' Title
    #' @name foo
    #' @eval foo()
    NULL
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))
})

test_that("must return non-NA string", {
  block <- "
    foo <- function() NA
    #' @eval foo()
    NULL
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))

  block <- "
    foo <- function() NA_character_
    #' @eval foo()
    NULL
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))
})

test_that("also works with namespace roclet", {
  out <- roc_proc_text(
    namespace_roclet(),
    "
    foo <- function() '@export a'
    #' @eval foo()
    #' @name a
    #' @title a
    NULL"
  )

  expect_equal(out, "export(a)")
})


# other -------------------------------------------------------------------

test_that("warns about duplicate tags", {
  block <- "
    #' Foo
    #' @rdname foo
    #' @rdname bar
    foo <- function() {}
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))
})
