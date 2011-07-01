context("Rd")
roc <- had_roclet()


test_that("@example loads from specified files", {
  out <- roc_proc_text(roc, "
    #' @name a
    #' @example Rd-example-1.R
    #' @example Rd-example-2.R
    NULL")[[1]]
  
  expect_equal(length(out), 4)
  expect_match(out, fixed("example <- 'example1'"), all = FALSE)
  expect_match(out, fixed("example <- 'example2'"), all = FALSE)
})

test_that("@examples captures examples", {
  out <- roc_proc_text(roc, "
    #' @name a
    #' @examples a <- 2
    NULL")[[1]]
  
  expect_equal(length(out), 4)
  expect_match(out, fixed("\\examples{a <- 2}"), all = FALSE)
})

test_that("@examples overrides @example", {
  out <- roc_proc_text(roc, "
    #' @name a
    #' @example expectedRd-example-1.R
    #' @examples a <- 2
    NULL")[[1]]
  expect_match(out, fixed("\\examples{a <- 2}"), all = FALSE)
})


test_that("empty file gives empty list", {
  out <- roc_proc_text(roc, "")
  expect_identical(out, list())
})

test_that("NULL gives empty list", {
  out <- roc_proc_text(roc, "NULL")
  expect_identical(out, list())
})


test_that("name captured from assignment", {
  out <- roc_proc_text(roc, "
    #' Title.
    a <- function() {} ")[[1]]

  expect_match(out, fixed("\\name{a}"), all = FALSE)
  expect_match(out, fixed("\\alias{a}"), all = FALSE)
  expect_match(out, "title[{]\\s*Title.\\s*[}]", all = FALSE)
})

test_that("@name overides default", {
  out <- roc_proc_text(roc, "
    #' @name b
    a <- function() {}")[[1]]
    
  expect_match(out, fixed("\\name{b}"), all = FALSE)
  expect_match(out, fixed("\\alias{b}"), all = FALSE)
  expect_match(out, fixed("\\title{b}"), all = FALSE)
})

test_that("usage captured from formals", {
  out <- roc_proc_text(roc, "
    #' Title.
    a <- function(a=1) {}")[[1]]
  expect_match(out, fixed("\\usage{a(a = 1)}"), all = FALSE)
})


test_that("@usage overrides default", {
  out <- roc_proc_text(roc, "
    #' @usage a(a=2)
    a <- function(a=1) {}")[[1]]
  expect_match(out, fixed("\\usage{a(a=2)}"), all = FALSE)
})


test_that("@param documents arguments", {
  out <- roc_proc_text(roc, "
    #' @param a an incipit letter
    #' @param z a terminal letter
    a <- function(a=1, z=2) {}")[[1]]
  expect_match(out, fixed("\\item{a}{an incipit letter"), all = FALSE)
  expect_match(out, fixed("\\item{z}{a terminal letter}"), all = FALSE)
})

test_that("description taken from first line", {
  out <- roc_proc_text(roc, "
    #' description
    #' @name a
    NULL")[[1]]
  expect_match(out, "description[{]\\s*description\\s*[}]", all = FALSE)
})

test_that("details taken from subsequent lines", {
  out <- roc_proc_text(roc, "
    #' description
    #'
    #' details
    #' @name a
    NULL")[[1]]
    expect_match(out, "description[{]\\s*description\\s*[}]", all = FALSE)
    expect_match(out, "details[{]\\s*details\\s*[}]", all = FALSE)
})

test_that("keywords and aliases split into pieces", {
  out <- roc_proc_text(roc, "
    #' @keywords a b
    #' @aliases a b
    #' @name a
    NULL")[[1]]
  expect_match(out, fixed("\\keyword{a}"), all = FALSE)
  expect_match(out, fixed("\\keyword{b}"), all = FALSE)
  expect_match(out, fixed("\\alias{a}"), all = FALSE)
  expect_match(out, fixed("\\alias{b}"), all = FALSE)
})

test_that("generic keys produce desired expected", {
  out <- roc_proc_text(roc, "
    #' @references test
    #' @note test
    #' @author test
    #' @seealso test
    #' @concept test
    #' @name a
    NULL")[[1]]
  expect_match(out, fixed("\\references{test}"), all = FALSE)
  expect_match(out, fixed("\\note{test}"), all = FALSE)
  expect_match(out, fixed("\\seealso{test}"), all = FALSE)
  expect_match(out, fixed("\\concept{test}"), all = FALSE)
  expect_match(out, fixed("\\author{test}"), all = FALSE)
})

test_that("title taken from first sentence", {
  out <- roc_proc_text(roc, "
    #' Description with sentence. That continueth.
    #' @name a
    NULL")[[1]]
  expect_match(out, fixed("\\title{Description with sentence.}"), all = FALSE)
  expect_match(out, all = FALSE, 
    fixed("Description with sentence. That continueth."))  
})

test_that("@title overrides default title", {
  out <- roc_proc_text(roc, "
    #' Would be title
    #' @title Overridden title
    #' @name a
    NULL")[[1]]
  expect_match(out, fixed("\\title{Overridden title}"), all = FALSE)
  expect_match(out, "description[{]\\s*Would be title", all = FALSE)
})


test_that("question mark ends sentence", {
  out <- roc_proc_text(roc, "
    #' Is a number odd?
    is.odd <- function(a) {}")[[1]]
  expect_match(out, fixed("\\title{Is a number odd?}"), all = FALSE)
})

test_that("no ending punctuation produces ellipsis", {
  out <- roc_proc_text(roc, "
    #' Whether a number is odd
    is.odd <- function(a) {}")[[1]]
  expect_match(out, fixed("\\title{Whether a number is odd...}"), all = FALSE)
})

test_that("@TODO creates todo section", {
  out <- roc_proc_text(roc, "
    #' @TODO test this
    #' @name a
    NULL")[[1]]
  expect_match(out, fixed("\\section{TODO}{test this}"), all = FALSE)
})
