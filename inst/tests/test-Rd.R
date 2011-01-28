context("Rd")

expect_rd <- function(roxygen, expected) {
  expected <- capture_roclet_output(make.Rd.roclet())
  expect_equal(expected, expected)
}

test_that("@example loads from specified files", {
  expect_rd("#' @example Rd-example-1.R
                   #' @example Rd-example-2.R
                   roxygen()",
                  expected=c("\\examples{example <- 'example1'",
                    "example <- 'example2'}"))
})

test_that("@examples captures examples", {
  expect_rd("#' @examples a <- 2
                   roxygen()",
                  expected="\\examples{a <- 2}")  
})

test_that("@examples overrides @example", {
  expect_rd("#' @example expectedRd-example-1.R
                   #' @examples a <- 2
                   roxygen()",
                  expected="\\examples{a <- 2}")  
})


test_that("empty file gives NULL expected", {
  output <- make.Rd.roclet()$parse.parsed(parse.text(""))
  expect_identical(output, NULL)
})

test_that("naked roxygen gives NULL expected", {
  output <- make.Rd.roclet()$parse.parsed(parse.text("roxygen()"))
  expect_identical(output, NULL)
})


test_that("name captured from assignment", {
  expect_rd('a <- 2',
                  expected=c('\\name{a}',
                    '\\alias{a}',
                    '\\title{a}'))  
})


test_that("@name overides default", {
  expect_rd("#' @name b
                   a <- 2",
                  expected=c('\\name{b}',
                    '\\alias{b}',
                    '\\title{b}'))
  
})

test_that("usage captured from formals", {
  expect_rd("a <- function(a=1) {}",
                  expected=c("\\name{a}",
                    "\\alias{a}",
                    "\\title{a}",
                    "\\usage{a(a=1)}"))  
})


test_that("@usage overrides default", {
  expect_rd("#' @usage a(a=2)
                   a <- function(a=1) {}",
                  expected=c("\\name{a}",
                    "\\alias{a}",
                    "\\title{a}",
                    "\\usage{a(a=2)}"))  
})


test_that("@param documents arguments", {
  expect_rd("#' @param a an incipit letter
                   #' @param z a terminal letter
                   a <- function(a=1, z=2) {}",
                  expected=c("\\name{a}",
                    "\\alias{a}",
                    "\\title{a}",
                    "\\usage{a(a=1, z=2)}",
                    "\\arguments{\\item{a}{an incipit letter}",
                    "\\item{z}{a terminal letter}}"))
})

test_that("description taken from first line", {
  expect_rd("#' description
                   roxygen()",
                  expected="\\description{description}")  
})

test_that("details taken from subsequent lines", {
  expect_rd("#' description
                   #'
                   #' details
                   roxygen()",
                  expected=c("\\description{description}",
                    "\\details{details}"))
  
})

test_that("keywords and aliases split into pieces", {
  expect_rd("#' @keywords a b
                   #' @aliases a b
                   roxygen()",
                  expected=c("\\keyword{a}",
                    "\\keyword{b}",
                    "\\alias{a}",
                    "\\alias{b}"))
})

test_that("generic keys produce desired expected", {
  expect_rd("#' @references test
                   #' @note test
                   #' @author test
                   #' @seealso test
                   #' @concept test
                   roxygen()",
                  expected=c("\\references{test}",
                    "\\note{test}",
                    "\\author{test}",
                    "\\seealso{test}",
                    "\\concept{test}"))  
})

test_that("title taken from first sentence", {
  expect_rd("#' Description with sentence. That continueth.
                   a <- 2",
                  expected=c("\\name{a}",
                    "\\alias{a}",
                    "\\title{Description with sentence.}",
                    paste("\\description{Description with sentence.",
                          "That continueth.}")))
  
})

test_that("@title overrides default title", {
  expect_rd("#' Would be title
                   #' @title Overridden title
                   roxygen()",
                  expected=c("\\title{Overridden title}",
                    "\\description{Would be title}"))  
})


test_that("question mark ends sentence", {
  expect_rd("#' Is a number odd?
                   is.odd <- function(a) {}",
                  expected=c('\\name{is.odd}',
                    '\\alias{is.odd}',
                    '\\title{Is a number odd?}',
                    '\\usage{is.odd(a)}',
                    '\\description{Is a number odd?}'))  
})

test_that("no ending punctuation produces ellipsis", {
  expect_rd("#' Whether a number is odd
                   is.odd <- function(a) {}",
                  expected=c('\\name{is.odd}',
                    '\\alias{is.odd}',
                    '\\title{Whether a number is odd...}',
                    '\\usage{is.odd(a)}',
                    '\\description{Whether a number is odd}'))  
})

test_that("@TODO creates todo section", {
  expect_rd("#' @TODO test this
                   roxygen()",
                  expected=c('\\section{TODO}{test this}'))
  
})
