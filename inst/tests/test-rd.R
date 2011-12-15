context("Rd")
roc <- rd_roclet()

# Names and filenames --------------------------------------------------------

test_that("name captured from assignment", {
  out <- roc_proc_text(roc, "
    #' Title.
    a <- function() {} ")[[1]]
  
  expect_equal(get_tag(out, "name")$values, "a")
  expect_equal(get_tag(out, "alias")$values, "a")
  expect_equal(get_tag(out, "title")$values, "Title.")
})

test_that("name also captured from assignment by =", {
  out <- roc_proc_text(roc, "
    #' Title.
    a = function() {} ")[[1]]
  
  expect_equal(get_tag(out, "name")$values, "a")
  expect_equal(get_tag(out, "alias")$values, "a")
  expect_equal(get_tag(out, "title")$values, "Title.")
})

test_that("names escaped, not quoted", {
  out <- roc_proc_text(roc, "
    #' Title
    '%a%' <- function(x, y) x + y")[[1]]
  expect_equal(format(get_tag(out, "name")), "\\name{\\%a\\%}\n")
})

test_that("filename doesn't contain invalid characters", {
  out <- roc_proc_text(roc, "
    #' Title.
    #' @name a<-
    NULL
    
    #' Title.
    #' @name a[]
    NULL")
  expect_equal(names(out), c("a-set.Rd", "a-sub.Rd"))
})

test_that("quoted names captured from assignment", {
  out <- roc_proc_text(roc, "
    #' Title.
    \"myfunction\" <- function(...) {}")[[1]]
  
  expect_equal(get_tag(out, "name")$values, "myfunction")
  expect_equal(get_tag(out, "alias")$values, "myfunction")
  
  out <- roc_proc_text(roc, "
    #' Title.
    `myfunction` <- function(...) {}")[[1]]
  expect_equal(get_tag(out, "name")$values, "myfunction")
  expect_equal(get_tag(out, "alias")$values, "myfunction")
  
  out <- roc_proc_text(roc, "
    #' Title.
    \"my function\" <- function(...) {}")[[1]]
  
  expect_equal(get_tag(out, "name")$values, "my function")
  expect_equal(get_tag(out, "alias")$values, "my function")
})

test_that("@name overides default", {
  out <- roc_proc_text(roc, "
    #' @name b
    a <- function() {}")[[1]]
    
    expect_equal(get_tag(out, "name")$values, "b")
    expect_equal(get_tag(out, "alias")$values, "b")
})

# Params  --------------------------------------------------------------------

test_that("@param documents arguments", {
  out <- roc_proc_text(roc, "
    #' @param a an incipit letter
    #' @param z a terminal letter
    a <- function(a=1, z=2) {}")[[1]]
    
  args <- get_tag(out, "arguments")$values  
  expect_equivalent(args["a"], "an incipit letter")
  expect_equivalent(args["z"], "a terminal letter")
})

test_that("multiple @inheritParam tags gathers all params", {
  out <- roc_process(roc, parse.files("Rd-params.R"), base_path = ".")
  
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

# Title, description, details ------------------------------------------------

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

test_that("docs parsed correctly if no blank text", {
  out <- roc_proc_text(roc, "
    #' @title My title
    #' @description My description
    #' @param x value
    a <- function(x) {}")[[1]]
  
  expect_equal(get_tag(out, "title")$values, "My title")
  expect_equal(get_tag(out, "description")$values, "My description")
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


# Keywords and aliases -------------------------------------------------------

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

test_that("aliases escaped, not quoted", {
  out1 <- roc_proc_text(roc, "
    #' @aliases a
    #' @name %a%
    NULL")[[1]]
  out2 <- roc_proc_text(roc, "
    #' @aliases %a%
    #' @name a
    NULL")[[1]]
  alias1 <- format(get_tag(out1, "alias"))
  alias2 <- format(get_tag(out2, "alias"))
  expect_equal(alias1, c("\\alias{\\%a\\%}\n", "\\alias{a}\n"))
  expect_equal(alias2, c("\\alias{\\%a\\%}\n", "\\alias{a}\n"))
})

# Keywords and aliases -------------------------------------------------------

test_that("empty file gives empty list", {
  out <- roc_proc_text(roc, "")
  expect_identical(out, list())
})

test_that("NULL gives empty list", {
  out <- roc_proc_text(roc, "NULL")
  expect_identical(out, list())
})


test_that("generic keys produce expected output", {
  out <- roc_proc_text(roc, "
    #' @references test
    #' @note test
    #' @author test
    #' @seealso test
    #' @concept test
    #' @encoding test
    #' @name a
    NULL")[[1]]
  expect_equal(get_tag(out, "references")$values, "test")
  expect_equal(get_tag(out, "note")$values, "test")
  expect_equal(get_tag(out, "seealso")$values, "test")
  expect_equal(get_tag(out, "concept")$values, "test")
  expect_equal(get_tag(out, "encoding")$values, "test")
  expect_equal(get_tag(out, "author")$values, "test")
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

test_that("`$` not to be parsed as assignee in foo$bar(a = 1)", {
  out <- roc_proc_text(roc, "
    #' foo object
    foo <- list(bar = function(a) a)
    foo$bar(a = 1)")[[1]]
    
    expect_equal(get_tag(out, "name")$values, "foo")
})

test_that("deleted objects not documented", {
  out <- roc_process(roc, parse.files("Rd-closure.R"), base_path = ".")
  expect_equal(names(out), "f2.Rd")
})

