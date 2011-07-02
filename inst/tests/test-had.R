context("Rd")
roc <- rd_roclet()


test_that("@example loads from specified files", {
  out <- roc_proc_text(roc, "
    #' @name a
    #' @example Rd-example-1.R
    #' @example Rd-example-2.R
    NULL")[[1]]
  
  expect_equal(length(out), 4)
  
  examples <- get_tag(out, "examples")$values
  expect_match(examples, fixed("example <- 'example1'"), all = FALSE)
  expect_match(examples, fixed("example <- 'example2'"), all = FALSE)
})

test_that("@examples captures examples", {
  out <- roc_proc_text(roc, "
    #' @name a
    #' @examples a <- 2
    NULL")[[1]]
  
  expect_equal(length(out), 4)
  examples <- get_tag(out, "examples")$values
  expect_match(examples, fixed("a <- 2"), all = FALSE)
})

test_that("@examples and @example combine", {
  out <- roc_proc_text(roc, "
    #' @name a
    #' @example Rd-example-1.R
    #' @examples a <- 2
    NULL")[[1]]

  examples <- get_tag(out, "examples")$values
  expect_match(examples, fixed("example <- 'example1'"), all = FALSE)
  expect_match(examples, fixed("a <- 2"), all = FALSE)
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
  
  expect_equal(get_tag(out, "name")$values, "a")
  expect_equal(get_tag(out, "alias")$values, "a")
  expect_equal(get_tag(out, "title")$values, "Title.")
})

test_that("@name overides default", {
  out <- roc_proc_text(roc, "
    #' @name b
    a <- function() {}")[[1]]
    
    expect_equal(get_tag(out, "name")$values, "b")
    expect_equal(get_tag(out, "alias")$values, "b")
    expect_equal(get_tag(out, "title")$values, "b")
})

test_that("usage captured from formals", {
  out <- roc_proc_text(roc, "
    #' Title.
    a <- function(a=1) {}")[[1]]
  expect_equal(get_tag(out, "usage")$values, "a(a = 1)")
})


test_that("@usage overrides default", {
  out <- roc_proc_text(roc, "
    #' @usage a(a=2)
    a <- function(a=1) {}")[[1]]
    expect_equal(get_tag(out, "usage")$values, "a(a=2)")
})


test_that("@param documents arguments", {
  out <- roc_proc_text(roc, "
    #' @param a an incipit letter
    #' @param z a terminal letter
    a <- function(a=1, z=2) {}")[[1]]
    
  args <- get_tag(out, "arguments")$values  
  expect_equivalent(args["a"], "an incipit letter")
  expect_equivalent(args["z"], "a terminal letter")
})

test_that("description taken from first line", {
  out <- roc_proc_text(roc, "
    #' description
    #' @name a
    NULL")[[1]]
  expect_equal(get_tag(out, "description")$values, "description")
})

test_that("details taken from subsequent lines", {
  out <- roc_proc_text(roc, "
    #' description
    #'
    #' details
    #' @name a
    NULL")[[1]]
  expect_equal(get_tag(out, "description")$values, "description")
  expect_equal(get_tag(out, "details")$values, "details")
})

test_that("keywords and aliases split into pieces", {
  out <- roc_proc_text(roc, "
    #' @keywords a b
    #' @aliases a b
    #' @name a
    NULL")[[1]]
    
  expect_match(get_tag(out, "keyword")$values, fixed("a"), all = FALSE)
  expect_match(get_tag(out, "keyword")$values, fixed("b"), all = FALSE)
  expect_match(get_tag(out, "alias")$values, fixed("a"), all = FALSE)
  expect_match(get_tag(out, "alias")$values, fixed("b"), all = FALSE)
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
  expect_equal(get_tag(out, "references")$values, "test")
  expect_equal(get_tag(out, "note")$values, "test")
  expect_equal(get_tag(out, "seealso")$values, "test")
  expect_equal(get_tag(out, "concept")$values, "test")
  expect_equal(get_tag(out, "author")$values, "test")
})

test_that("title taken from first sentence", {
  out <- roc_proc_text(roc, "
    #' Description with sentence. That continueth.
    #' @name a
    NULL")[[1]]
  expect_equal(get_tag(out, "title")$values, "Description with sentence.")
  expect_equal(get_tag(out, "description")$values, 
    "Description with sentence. That continueth.")
})

test_that("@title overrides default title", {
  out <- roc_proc_text(roc, "
    #' Would be title
    #' @title Overridden title
    #' @name a
    NULL")[[1]]
  expect_equal(get_tag(out, "title")$values, "Overridden title")
  expect_equal(get_tag(out, "description")$values, "Would be title")
})


test_that("question mark ends sentence", {
  out <- roc_proc_text(roc, "
    #' Is a number odd?
    is.odd <- function(a) {}")[[1]]
  expect_equal(get_tag(out, "title")$values, "Is a number odd?")
  
})

test_that("no ending punctuation produces ellipsis", {
  out <- roc_proc_text(roc, "
    #' Whether a number is odd
    is.odd <- function(a) {}")[[1]]
  expect_equal(get_tag(out, "title")$values, "Whether a number is odd...")
})
