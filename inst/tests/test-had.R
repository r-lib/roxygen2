context("Rd")
roc <- rd_roclet()


test_that("@example loads from specified files", {
  out <- roc_proc_text(roc, "
    #' @name a
    #' @example Rd-example-1.R
    #' @example Rd-example-2.R
    NULL")[[1]]
  
  examples <- get_tag(out, "examples")$values
  expect_match(examples, fixed("example <- 'example1'"), all = FALSE)
  expect_match(examples, fixed("example <- 'example2'"), all = FALSE)
})

test_that("@examples captures examples", {
  out <- roc_proc_text(roc, "
    #' @name a
    #' @examples a <- 2
    NULL")[[1]]
  
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
})

test_that("usage captured from formals", {
  out <- roc_proc_text(roc, "
    #' Title.
    a <- function(a=1) {}")[[1]]
  expect_equal(get_tag(out, "usage")$values, "a(a = 1)")
})

test_that("% is escaped in usage", {
  out <- roc_proc_text(roc, "
    #' Title.
    a <- function(a='%') {}")[[1]]
  expect_equal(get_tag(out, "usage")$values, "a(a = \"\\%\")")
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

test_that("title and description taken from first line if only one", {
  out <- roc_proc_text(roc, "
    #' description
    #' @name a
    NULL")[[1]]
  expect_equal(get_tag(out, "description")$values, "description")
  expect_equal(get_tag(out, "title")$values, "description")
})

test_that("title, description and details extracted correctly", {
  out <- roc_proc_text(roc, "
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

test_that("title taken from first paragraph", {
  out <- roc_proc_text(roc, "
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
  out <- roc_proc_text(roc, "
    #' Would be title
    #' @title Overridden title
    #' @name a
    NULL")[[1]]
  expect_equal(get_tag(out, "title")$values, "Overridden title")
  expect_equal(get_tag(out, "description")$values, "Would be title")
})

test_that("@noRd inhibits documentation", {
  out <- roc_proc_text(roc, "
    #' Would be title
    #' @title Overridden title
    #' @name a
    #' @noRd
    NULL")
  
  expect_equal(length(out), 0)
})

test_that("question mark ends sentence", {
  out <- roc_proc_text(roc, "
    #' Is a number odd?
    is.odd <- function(a) {}")[[1]]
  expect_equal(get_tag(out, "title")$values, "Is a number odd?")
  
})

test_that("no ending punctuation does not produce ellipsis", {
  out <- roc_proc_text(roc, "
    #' Whether a number is odd
    is.odd <- function(a) {}")[[1]]
  expect_equal(get_tag(out, "title")$values, "Whether a number is odd")
})


test_that("multiple @inheritParam tags gathers all params", {
  out <- roc_process(roc, parse.files("Rd-collate.R"), base_path = ".")
  
  params <- get_tag(out[["c.Rd"]], "arguments")$values
  expect_equal(length(params), 2)
  
  expect_equal(params[["x"]], "X")
  expect_equal(params[["y"]], "Y")  
})

test_that("multiple @inheritParam inherits from existing topics", {
  out <- roc_proc_text(roc, "
    #' My mean
    #' 
    #' @inheritParams base::mean
    mymean <- function(x, trim) {}")[[1]]
  params <- get_tag(out, "arguments")$values
  expect_equal(length(params), 2)
  expect_equal(sort(names(params)), c("trim", "x"))
})