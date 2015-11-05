context("Markdown markup")
roc <- rd_roclet()

test_that("backticks are converted to \\code", {
  out1 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description with some `code` included. `More code.`
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description with some \\code{code} included. \\code{More code.}
    foo <- function() {}")[[1]]
  expect_equal(get_tag(out1, "description"), get_tag(out2, "description"))
})

test_that("code blocks work", {
  out1 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description
    #'
    #' Details with a code block:
    #' ```r
    #' x <- 1:10 %>%
    #' multiply_by(10) %>%
    #' add(42)
    #' ```
    #' Normal text again.
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description
    #'
    #' Details with a code block:\\preformatted{x <- 1:10 %>%
    #' multiply_by(10) %>%
    #' add(42)
    #' }
    #'
    #' Normal text again.
    foo <- function() {}")[[1]]
  expect_equal(get_tag(out1, "details"), get_tag(out2, "details"))
})

test_that("itemized lists work", {
  out1 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description with some
    #' * itemized
    #' * list
    #'
    #' And then another one:
    #' * item 1
    #' * item 2
    #' * item 3
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description with some
    #' \\itemize{
    #' \\item itemized
    #' \\item list
    #' }
    #'
    #' And then another one:
    #' \\itemize{
    #' \\item item 1
    #' \\item item 2
    #' \\item item 3
    #' }
    foo <- function() {}")[[1]]
  expect_equal(get_tag(out1, "description"), get_tag(out2, "description"))
  expect_equal(get_tag(out1, "details"), get_tag(out2, "details"))
})

test_that("numbered lists work", {
  out1 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description with some
    #' 1. numbered
    #' 2. list
    #'
    #' And then another one:
    #' 1. item 1
    #' 1. item 2
    #' 1. item 3
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description with some
    #' \\enumerate{
    #' \\item numbered
    #' \\item list
    #' }
    #'
    #' And then another one:
    #' \\enumerate{
    #' \\item item 1
    #' \\item item 2
    #' \\item item 3
    #' }
    foo <- function() {}")[[1]]
  expect_equal(get_tag(out1, "description"), get_tag(out2, "description"))
  expect_equal(get_tag(out1, "details"), get_tag(out2, "details"))
})

test_that("nested lists are OK", {
  out1 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description with some
    #' 1. numbered
    #'    * itemized
    #'    * sublist
    #' 2. list
    #'
    #' And then another one:
    #' * item 1
    #' * item 2
    #'    * sublist
    #'    * within
    #' * item 3
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description with some
    #' \\enumerate{
    #' \\item numbered
    #'   \\itemize{
    #'      \\item itemized
    #'      \\item sublist
    #'   }
    #' \\item list
    #' }
    #'
    #' And then another one:
    #' \\itemize{
    #' \\item item 1
    #' \\item item 2
    #'    \\itemize{
    #'      \\item sublist
    #'      \\item within
    #'    }
    #' \\item item 3
    #' }
    foo <- function() {}")[[1]]
  expect_equal(get_tag(out1, "description"), get_tag(out2, "description"))
  expect_equal(get_tag(out1, "details"), get_tag(out2, "details"))

})

test_that("emphasis works", {
  out1 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description with some _emphasis_ included. _More emph._
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description with some \\emph{emphasis} included. \\emph{More emph.}
    foo <- function() {}")[[1]]
  expect_equal(get_tag(out1, "description"), get_tag(out2, "description"))
})

test_that("strong (bold) text works", {
  out1 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description with some **bold** included. **More bold.**
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description with some \\strong{bold} included. \\strong{More bold.}
    foo <- function() {}")[[1]]
  expect_equal(get_tag(out1, "description"), get_tag(out2, "description"))
})

test_that("links work", {
  ## TODO
})

test_that("markdown links are converted", {
  out1 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description, see [http://acme.com]() for details.
    #' And here is a named link: [igraph](http://igraph.org).
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description, see \\url{http://acme.com} for details.
    #' And here is a named link: \\href{http://igraph.org}{igraph}.
    foo <- function() {}")[[1]]
  expect_equal(get_tag(out1, "description"), get_tag(out2, "description"))
})

test_that("images are recognized", {
  out1 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description
    #'
    #' Details with a plot: ![](example.jpg \"Plot title\")
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description
    #'
    #' Details with a plot: \\figure{example.jpg}{Plot title}
    foo <- function() {}")[[1]]
  expect_equal(get_tag(out1, "description"), get_tag(out2, "description"))
})

test_that("markdown is parsed in all fields where it is supported", {
  out1 <- roc_proc_text(roc, "
    #' @title Title **with bold**
    #'
    #' @description Description **with bold**
    #'
    #' @details Details **with bold**
    #'
    #' @references References **with bold**
    #'
    #' @concept Concept **with bold**
    #'
    #' @note Note **with bold**
    #'
    #' @seealso See also **with bold**
    #'
    #' @keywords Keywords **with bold**
    #'
    #' @return Return **with bold**
    #'
    #' @author Author **with bold**
    #'
    #' @section Foobar:
    #' With some **bold text**.
    #'
    #' @format Format **with bold**
    #'
    #' @source Source **with bold**
    #'
    #' @param param Param **with bold**
    #'
    #' @slot slot Slot **with bold**
    #'
    #' @field field Field **with bold**
    #'
    #' @method method Method **with bold**
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' @title Title \\strong{with bold}
    #'
    #' @description Description \\strong{with bold}
    #'
    #' @details Details \\strong{with bold}
    #'
    #' @references References \\strong{with bold}
    #'
    #' @concept Concept \\strong{with bold}
    #'
    #' @note Note \\strong{with bold}
    #'
    #' @seealso See also \\strong{with bold}
    #'
    #' @keywords Keywords \\strong{with bold}
    #'
    #' @return Return \\strong{with bold}
    #'
    #' @author Author \\strong{with bold}
    #'
    #' @section Foobar:
    #' With some **bold text**.
    #'
    #' @format Format \\strong{with bold}
    #'
    #' @source Source \\strong{with bold}
    #'
    #' @param param Param \\strong{with bold}
    #'
    #' @slot slot Slot \\strong{with bold}
    #'
    #' @field field Field \\strong{with bold}
    #'
    #' @method method Method \\strong{with bold}
    foo <- function() {}")[[1]]
  expect_equal(get_tag(out1, "title"), get_tag(out2, "title"))
  expect_equal(get_tag(out1, "description"), get_tag(out2, "description"))
  expect_equal(get_tag(out1, "details"), get_tag(out2, "details"))
  expect_equal(get_tag(out1, "references"), get_tag(out2, "references"))
  expect_equal(get_tag(out1, "concept"), get_tag(out2, "concept"))
  expect_equal(get_tag(out1, "note"), get_tag(out2, "note"))
  expect_equal(get_tag(out1, "seealso"), get_tag(out2, "seealso"))
  expect_equal(get_tag(out1, "keywords"), get_tag(out2, "keywords"))
  expect_equal(get_tag(out1, "return"), get_tag(out2, "return"))
  expect_equal(get_tag(out1, "author"), get_tag(out2, "author"))
  expect_equal(get_tag(out1, "section"), get_tag(out2, "section"))
  expect_equal(get_tag(out1, "format"), get_tag(out2, "format"))
  expect_equal(get_tag(out1, "source"), get_tag(out2, "source"))
  expect_equal(get_tag(out1, "param"), get_tag(out2, "param"))
  expect_equal(get_tag(out1, "slot"), get_tag(out2, "slot"))
  expect_equal(get_tag(out1, "field"), get_tag(out2, "field"))
  expect_equal(get_tag(out1, "method"), get_tag(out2, "method"))
})
