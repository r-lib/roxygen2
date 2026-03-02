# code --------------------------------------------------------------------
# see test-markdown-code.R for evaluated code

test_that("backticks are converted to \\code & \\verb", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description with some `code` included. `More code.`
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description with some \\code{code} included. \\verb{More code.}
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("code blocks work", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' @description
    #' Before
    #' ```
    #' x %in% 1:10
    #' ```
    #' After
    #' @md
    foo <- function() {}"
  )[[1]]
  expect_snapshot_output(cat(out1$get_value("description")))

  # And check that extra empty paragraphs don't affect the output
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' @description
    #' Before
    #'
    #' ```
    #' x %in% 1:10
    #' ```
    #'
    #' After
    #' @md
    foo <- function() {}"
  )[[1]]
  expect_equal(out1$get_value("description"), out2$get_value("description"))
})

test_that("code block with language creates HTML tag", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' @description
    #' Before
    #' ```r
    #' x %in% 1:10
    #' ```
    #' After
    #' @md
    foo <- function() {}"
  )[[1]]
  expect_snapshot_output(cat(out1$get_value("description")))
})

test_that("inline code escapes %", {
  expect_equal(markdown("`5%`"), r"(\verb{5\%})")
  expect_equal(markdown("`'5%'`"), r"(\code{'5\%'})")
  expect_equal(markdown("`%*%`"), r"(\code{\%*\%})")
})

test_that("inline verbatim escapes Rd special chars", {
  expect_equal(markdown("`{`"), r"(\verb{\{})")
  expect_equal(markdown("`}`"), r"(\verb{\}})")
  expect_equal(markdown("`\\`"), r"(\verb{\\})")
})

test_that("special operators get \\code{}, not \\verb{}", {
  expect_equal(markdown("`if`"), "\\code{if}")
})

test_that("inline code works with < and >", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' `SELECT <name> FROM <table>`
    #' @md
    f <- function() 1
  "
  )[[1]]

  expect_equal(out$get_value("title"), r"(\verb{SELECT <name> FROM <table>})")
})


# lists -------------------------------------------------------------------

test_that("itemized lists work", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
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
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
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
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("numbered lists work", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
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
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
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
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("nested lists are OK", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
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
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
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
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})


# html --------------------------------------------------------------------

test_that("can insert block and inline html", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' <p>This is a paragraph</p>
    #' <p>This is another paragraph</p>
    #' @md
    foo <- function() {}"
  )[[1]]
  expect_snapshot(out$get_section("description"))

  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' This is a paragraph containing a manually inserted image
    #' before-<img src='foo.png'>-after
    #' @md
    foo <- function() {}"
  )[[1]]
  expect_snapshot(out$get_section("description"))
})

# tables ------------------------------------------------------------------

test_that("can convert table to Rd", {
  txt <- "
    | x   | y   |
    | --- | --- |
    | 1   | 2   |

    | x   | y   |
    | :-: | --: |
    | 1   | 2   |

    | x     | y         |
    | ----- | --------- |
    | 1 _2_ | 3 *4* `5` |
  "
  txt <- gsub("\n    ", "\n", txt)
  tables <- strsplit(txt, "\n\n")[[1]]

  expect_snapshot({
    for (table in tables) {
      cat_line(table)
      cat_line(markdown(table))
      cat_line()
    }
  })
})

# inline formatting -------------------------------------------------------

test_that("emphasis works", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description with some _emphasis_ included. _More emph._
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description with some \\emph{emphasis} included. \\emph{More emph.}
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("strong (bold) text works", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description with some **bold** included. **More bold.**
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description with some \\strong{bold} included. \\strong{More bold.}
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("markdown links are converted", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see [http://acme.com]() for details.
    #' And here is a named link: [igraph](http://igraph.org).
    #' Here is another kind of link: <https://log.r-hub.io>.
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see \\url{http://acme.com} for details.
    #' And here is a named link: \\href{http://igraph.org}{igraph}.
    #' Here is another kind of link: \\url{https://log.r-hub.io}.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("images are recognized", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description
    #'
    #' Details with a plot: ![](example.jpg \"Plot title\")
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description
    #'
    #' Details with a plot: \\figure{example.jpg}{Plot title}
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("markdown is parsed in all fields where it is supported", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' @title Title **with bold**
    #'
    #' @description Description **with bold**
    #'
    #' @details Details **with bold**
    #'
    #' @references References **with bold**
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
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' @title Title \\strong{with bold}
    #'
    #' @description Description \\strong{with bold}
    #'
    #' @details Details \\strong{with bold}
    #'
    #' @references References \\strong{with bold}
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
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})


test_that("markdown emphasis is ok", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description with some *keywords* included.
    #' So far so good. \\preformatted{ *these are not
    #'   emphasised*. Or are they?
    #' }
    #' @md
    foo <- function() {}"
  )[[1]]
  desc1 <- r"(Description with some \emph{keywords} included.
So far so good. \preformatted{ *these are not
  emphasised*. Or are they?
})"
  expect_equal(out1$get_section("description")[[2]], desc1)
})

test_that("% is automatically escaped", {
  expect_equal(markdown("5%"), r"(5\%)")
})

test_that("Escaping is kept", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description. It has \\rd \\commands.
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description. It has \\rd \\commands.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("Do not pick up `` in arguments \\item #519", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description.
    #'
    #' @param `_arg1` should not be code. But `this should`.
    #' @param `_arg2` should not be code, either. `But this.`
    #'
    #' @md
    foo <- function(`_arg1`, `_arg2`) {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description.
    #'
    #' @param `_arg1` should not be code. But \\verb{this should}.
    #' @param `_arg2` should not be code, either. \\verb{But this.}
    #'
    foo <- function(`_arg1`, `_arg2`) {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("unhandled markdown generates warning", {
  text <- "
    #' Title
    #'
    #' > block quotes do not work,
    #' > sadly
    #'
    #' Blabla
    #' @md
    #' @name x
    NULL
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), text))
})

test_that("level 1 heading in markdown generates warning in some tags", {
  text <- "
    #' Title
    #'
    #' @seealso this and that
    #' # This is not good
    #'
    #' Blabla
    #' @md
    #' @name x
    NULL
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), text))
})

test_that("level >2 markdown headings work in @description", {
  text <- "
    #' Title
    #'
    #' @description
    #' ## This is good
    #' yes
    #'
    #' @details
    #' Blabla
    #' @md
    #' @name x
    NULL
  "
  out <- roc_proc_text(rd_roclet(), text)[[1]]
  expect_equal_strings(
    out$get_value("description"),
    r"(\subsection{This is good}{

yes
})"
  )
})

test_that("level >2 markdown headings work in @details", {
  text <- "
    #' Title
    #'
    #' Description.
    #'
    #' @details
    #' ## Heading 2
    #' ### Heading 3
    #' Text.
    #' @md
    #' @name x
    NULL
  "
  out <- roc_proc_text(rd_roclet(), text)[[1]]
  expect_equal_strings(
    out$get_value("details"),
    r"(\subsection{Heading 2}{
\subsection{Heading 3}{

Text.
}

})"
  )
})

test_that("level >2 markdown headings work in @return", {
  text <- "
    #' Title
    #'
    #' Description.
    #'
    #' @return Even this
    #' ## Can have a subsection.
    #' Yes.
    #' @md
    #' @name x
    NULL
  "
  out <- roc_proc_text(rd_roclet(), text)[[1]]
  expect_equal_strings(
    out$get_value("value"),
    r"(Even this
\subsection{Can have a subsection.}{

Yes.
})"
  )
})

test_that("level 1 heading in @details", {
  text1 <- "
    #' Title
    #'
    #' Description.
    #'
    #' @details
    #' Leading text goes into details.
    #' # This is its own section
    #' ## Can have a subsection
    #' Yes.
    #' # Another section
    #' With text.
    #' @md
    #' @name x
    NULL
  "
  out1 <- roc_proc_text(rd_roclet(), text1)[[1]]
  text2 <- "
    #' Title
    #'
    #' Description.
    #' @details
    #' Leading text goes into details.
    #' @section This is its own section:\\subsection{Can have a subsection}{
    #'
    #' Yes.
    #' }
    #'
    #' @section Another section:With text.
    #' @name x
    NULL
  "
  out2 <- roc_proc_text(rd_roclet(), text2)[[1]]

  # make sure sections are in the same order
  expect_equal(sort(names(out1$sections)), sort(names(out2$sections)))
  out2$sections <- out2$sections[names(out1$sections)]

  expect_equivalent_rd(out1, out2)
})

test_that("headings and empty sections", {
  text1 <- "
    #' Title
    #'
    #' Description.
    #'
    #' @details
    #' # This is its own section
    #' With text.
    #' @md
    #' @name x
    NULL
  "
  out1 <- roc_proc_text(rd_roclet(), text1)[[1]]
  expect_false("details" %in% names(out1$fields))
})

test_that("markdown() on empty input", {
  expect_identical(markdown(""), "")
  expect_identical(markdown("  "), "")
  expect_identical(markdown("\n"), "")
})

test_that("markup in headings", {
  text1 <- "
    #' Title
    #'
    #' Description.
    #'
    #' @details
    #' Leading text goes into details.
    #' # Section with `code`
    #' ## Subsection with **strong**
    #' Yes.
    #' @md
    #' @name x
    NULL
  "
  out1 <- roc_proc_text(rd_roclet(), text1)[[1]]
  expect_equal(
    out1$get_value("section"),
    list(
      title = "Section with \\code{code}",
      content = paste(
        sep = "\n",
        "\\subsection{Subsection with \\strong{strong}}{",
        "",
        "Yes.",
        "}"
      )
    )
  )
})

test_that("alternative knitr engines", {
  expect_snapshot(
    print(
      out1 <- roc_proc_text(
        rd_roclet(),
        "
      #' Title
      #'
      #' Description.
      #'
      #' ```{verbatim}
      #' #| file = testthat::test_path(\"example.Rmd\")
      #' ```
      #' @md
      #' @name x
      NULL
    "
      )
    )
  )
})

test_that("can override default options", {
  local_roxy_meta_set("knitr_chunk_options", list(comment = "###"))

  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' ```{r}
    #' 1+1
    #' ```
    #' @md
    foo <- function() { }
  "
  )[[1]]
  expect_match(out$get_section("description")$value, "###", fixed = TRUE)
})

test_that("image formats work", {
  expect_snapshot(
    roc_proc_text(
      rd_roclet(),
      "
      #' Title
      #'
      #' ![](example.svg \"Plot title 1\")
      #' ![](example.pdf \"Plot title 2\")
      #' ![](example.PNG \"Plot title 3\")
      #' @md
      foo <- function() { }
    "
    )[[1]]
  )
})
