# code --------------------------------------------------------------------

test_that("backticks are converted to \\code & \\verb", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description with some `code` included. `More code.`
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description with some \\code{code} included. \\verb{More code.}
    foo <- function() {}")[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("code blocks work", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description
    #'
    #' Details with a code block:
    #' ```r
    #' x <- 1:10 %>%
    #'   multiply_by(10) %>%
    #'   add(42)
    #' ```
    #' Normal text again.
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description
    #'
    #' Details with a code block:\\preformatted{x <- 1:10 \\%>\\%
    #'   multiply_by(10) \\%>\\%
    #'   add(42)
    #' }
    #'
    #' Normal text again.
    foo <- function() {}")[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("inline code escapes %", {
  expect_equal(markdown("`5%`"), "\\verb{5\\%}")
  expect_equal(markdown("`'5%'`"), "\\code{'5\\%'}")
})

test_that("inline verbatim escapes Rd special chars", {
  expect_equal(markdown("`{`"), "\\verb{\\{}")
  expect_equal(markdown("`}`"), "\\verb{\\}}")
  expect_equal(markdown("`\\`"), "\\verb{\\\\}")
})

test_that("code blocks escape %", {
  expect_equal(
    markdown("```\n1:10 %>% mean()\n```"),
    "\\preformatted{1:10 \\%>\\% mean()\n}"
  )
})

test_that("inline code works with < and >", {
  out <- roc_proc_text(rd_roclet(), "
    #' `SELECT <name> FROM <table>`
    #' @md
    f <- function() 1
  ")[[1]]

  expect_equal(out$get_field("title")$values, "\\verb{SELECT <name> FROM <table>}")
})


# lists -------------------------------------------------------------------

test_that("itemized lists work", {
  out1 <- roc_proc_text(rd_roclet(), "
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
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(rd_roclet(), "
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
  expect_equivalent_rd(out1, out2)
})

test_that("numbered lists work", {
  out1 <- roc_proc_text(rd_roclet(), "
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
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(rd_roclet(), "
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
  expect_equivalent_rd(out1, out2)
})

test_that("nested lists are OK", {
  out1 <- roc_proc_text(rd_roclet(), "
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
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(rd_roclet(), "
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
    #' @md
    foo <- function() {}")[[1]]
  expect_equivalent_rd(out1, out2)
})


# inline formatting -------------------------------------------------------

test_that("emphasis works", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description with some _emphasis_ included. _More emph._
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description with some \\emph{emphasis} included. \\emph{More emph.}
    foo <- function() {}")[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("strong (bold) text works", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description with some **bold** included. **More bold.**
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description with some \\strong{bold} included. \\strong{More bold.}
    foo <- function() {}")[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("markdown links are converted", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description, see [http://acme.com]() for details.
    #' And here is a named link: [igraph](http://igraph.org).
    #' Here is another kind of link: <https://log.r-hub.io>.
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description, see \\url{http://acme.com} for details.
    #' And here is a named link: \\href{http://igraph.org}{igraph}.
    #' Here is another kind of link: \\url{https://log.r-hub.io}.
    foo <- function() {}")[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("images are recognized", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description
    #'
    #' Details with a plot: ![](example.jpg \"Plot title\")
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description
    #'
    #' Details with a plot: \\figure{example.jpg}{Plot title}
    foo <- function() {}")[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("markdown is parsed in all fields where it is supported", {
  out1 <- roc_proc_text(rd_roclet(), "
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
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(rd_roclet(), "
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
    #' @return Return \\strong{with bold}
    #'
    #' @author Author \\strong{with bold}
    #'
    #' @section Foobar:
    #' With some \\strong{bold text}.
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
    foo <- function() {}")[[1]]
  expect_equivalent_rd(out1, out2)
})


test_that("markdown emphasis is ok", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description with some *keywords* included.
    #' So far so good. \\preformatted{ *these are not
    #'   emphasised*. Or are they?
    #' }
    #' @md
    foo <- function() {}")[[1]]
  desc1 <- "Description with some \\emph{keywords} included.
So far so good. \\preformatted{ *these are not
  emphasised*. Or are they?
}"
  expect_equal(get_tag(out1, "description")[[2]], desc1)
})

test_that("% is automatically escaped", {
  expect_equal(markdown("5%"), "5\\%")
})

test_that("% and $ and _ are not unescaped", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' @description \\% \\$ \\_.
    #' @param foo \\% \\$ \\_.
    #' @md
    foo <- function(foo) {}")[[1]]
  out2 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' @description \\\\% \\$ \\_.
    #' @param foo \\\\% \\$ \\_.
    foo <- function(foo) {}")[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("Escaping is kept", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description. It has \\rd \\commands.
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description. It has \\rd \\commands.
    foo <- function() {}")[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("Do not pick up `` in arguments \\item #519", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description.
    #'
    #' @param `_arg1` should not be code. But `this should`.
    #' @param `_arg2` should not be code, either. `But this.`
    #'
    #' @md
    foo <- function(`_arg1`, `_arg2`) {}")[[1]]
  out2 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description.
    #'
    #' @param `_arg1` should not be code. But \\verb{this should}.
    #' @param `_arg2` should not be code, either. \\verb{But this.}
    #'
    foo <- function(`_arg1`, `_arg2`) {}")[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("unhandled markdown generates warning", {
  text <- "
    #' Title
    #'
    #' ## Heading
    #'
    #' Blabla
    #' @md
    #' @name x
    NULL
  "
  expect_warning(roc_proc_text(rd_roclet(), text), "markdown headings")
})