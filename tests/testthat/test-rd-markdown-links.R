context("Rd: markdown links")
roc <- rd_roclet()

test_that("proper link references are added", {
  cases <- list(
    c("foo [func()] bar",           "[func()]: R:func()"),
    c("foo [obj] bar",              "[obj]: R:obj"),
    c("foo [text][func()] bar",     "[func()]: R:func()"),
    c("foo [text][obj] bar",        "[obj]: R:obj"),
    c("foo [pkg::func()] bar",       "[pkg::func()]: R:pkg::func()"),
    c("foo [pkg::obj] bar",          "[pkg::obj]: R:pkg::obj"),
    c("foo [text][pkg::func()] bar", "[pkg::func()]: R:pkg::func()"),
    c("foo [text][pkg::obj] bar",    "[pkg::obj]: R:pkg::obj")
  )

  for (i in seq_along(cases)) {
    expect_match(
      add_linkrefs_to_md(cases[[i]][1]),
      cases[[i]][2],
      fixed = TRUE
    )
  }
})

test_that("commonmark picks up the various link references", {
  cases <- list(
    c("foo [func()] bar",
      "<link destination=\"R:func\\(\\)\" title=\"\">\\s*<text>func\\(\\)</text>"),
    c("foo [obj] bar",
      "<link destination=\"R:obj\" title=\"\">\\s*<text>obj</text>"),
    c("foo [text][func()] bar",
      "<link destination=\"R:func\\(\\)\" title=\"\">\\s*<text>text</text>"),
    c("foo [text][obj] bar",
      "<link destination=\"R:obj\" title=\"\">\\s*<text>text</text>"),
    c("foo [pkg::func()] bar",
      "<link destination=\"R:pkg::func\\(\\)\" title=\"\">\\s*<text>pkg::func\\(\\)</text>"),
    c("foo [pkg::obj] bar",
      "<link destination=\"R:pkg::obj\" title=\"\">\\s*<text>pkg::obj</text>"),
    c("foo [text][pkg::func()] bar",
      "<link destination=\"R:pkg::func\\(\\)\" title=\"\">\\s*<text>text</text>"),
    c("foo [text][pkg::obj] bar",
      "<link destination=\"R:pkg::obj\" title=\"\">\\s*<text>text</text>")
  )

  for (i in seq_along(cases)) {
    expect_match(
      commonmark::markdown_xml(add_linkrefs_to_md(cases[[i]][1])),
      cases[[i]][2]
    )
  }
})

test_that("short and sweet links work", {
  out1 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description, see [function()].
    #' And also [object].
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description, see \\code{\\link[=function]{function()}}.
    #' And also \\link{object}.
    foo <- function() {}")[[1]]
  expect_equal(get_tag(out1, "description"), get_tag(out2, "description"))

  out1 <- roc_proc_text(roc, "
    #' Title
    #'
    #' See [pkg::function()], [pkg::object].
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' Title
    #'
    #' See \\code{\\link[pkg:function]{pkg::function()}}, \\link[pkg:object]{pkg::object}.
    foo <- function() {}")[[1]]
  expect_equal(get_tag(out1, "description"), get_tag(out2, "description"))

  out1 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description, see [name][dest].
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description, see \\link[=dest]{name}.
    foo <- function() {}")[[1]]
  expect_equal(get_tag(out1, "description"), get_tag(out2, "description"))

  out1 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description, see [name words][pkg::bar].
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description, see \\link[pkg:bar]{name words}.
    foo <- function() {}")[[1]]
  expect_equal(get_tag(out1, "description"), get_tag(out2, "description"))

  out1 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description, see [terms][terms.object].
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description, see \\link[=terms.object]{terms}.
    foo <- function() {}")[[1]]
  expect_equal(get_tag(out1, "description"), get_tag(out2, "description"))

  out1 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description, see [abc][abc-class].
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description, see \\link[=abc-class]{abc}.
    foo <- function() {}")[[1]]
  expect_equal(get_tag(out1, "description"), get_tag(out2, "description"))

  out1 <- roc_proc_text(roc, "
    #' In another package: [and this one][devtools::document].
    #' [name words][devtools::document].
    #'
    #' @md
    #' @name markdown-test
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' In another package: \\link[devtools:document]{and this one}.
    #' \\link[devtools:document]{name words}.
    #'
    #' @md
    #' @name markdown-test
    foo <- function() {}")[[1]]
})

test_that("a weird markdown link bug is fixed", {

  out1 <- roc_proc_text(roc, "
    #' Dummy page to test roxygen's markdown formatting
    #'
    #' Links are very tricky, so I'll put in some links here:
    #' Link to a function: [roxygenize()].
    #' Link to an object: [roxygenize] (we just treat it like an object here).
    #'
    #' Link to another package, function: [devtools::document()].
    #' Link to another package, non-function: [devtools::document].
    #'
    #' Link with link text:  [this great function][roxygenize()] or
    #' [that great function][roxygenize].
    #'
    #' In another package: [and this one][devtools::document].
    #'
    #' @md
    #' @name markdown-test
    #' @keywords internal
    NULL")[[1]]
  out2 <- roc_proc_text(roc, "
    #' Dummy page to test roxygen's markdown formatting
    #'
    #' Links are very tricky, so I'll put in some links here:
    #' Link to a function: \\code{\\link[=roxygenize]{roxygenize()}}.
    #' Link to an object: \\link{roxygenize} (we just treat it like an object here).
    #'
    #' Link to another package, function: \\code{\\link[devtools:document]{devtools::document()}}.
    #' Link to another package, non-function: \\link[devtools:document]{devtools::document}.
    #'
    #' Link with link text:  \\link[=roxygenize]{this great function} or
    #' \\link[=roxygenize]{that great function}.
    #'
    #' In another package: \\link[devtools:document]{and this one}.
    #'
    #' @name markdown-test
    #' @keywords internal
    NULL")[[1]]
  expect_equal(get_tag(out1, "description"), get_tag(out2, "description"))
  expect_equal(get_tag(out1, "details"), get_tag(out2, "details"))
})

test_that("another markdown link bug is fixed", {

  out1 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description, see [escape_rd_for_md()].
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description, see \\code{\\link[=escape_rd_for_md]{escape_rd_for_md()}}.
    #'
    #' And also \\link{object}.
    foo <- function() {}")[[1]]
  expect_equal(get_tag(out1, "description"), get_tag(out2, "description"))
})

test_that("non-code link in backticks works", {

  out1 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description, see [`foobar`].
    #' Also [`this_too`].
    #' @md
    foo <- function() {}")[[1]]
  out2 <- roc_proc_text(roc, "
    #' Title
    #'
    #' Description, see \\code{\\link{foobar}}.
    #' Also \\code{\\link{this_too}}.
    foo <- function() {}")[[1]]
  expect_equal(get_tag(out1, "description"), get_tag(out2, "description"))
})
