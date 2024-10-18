
## fix 1670 ----

.expect_inherited = function(x, params) {
  names(params)[names(params) == "\\dots"] = "..."
  strings = sprintf("\\item{\\code{%s}}{%s}",names(params),params)
  found = stringr::str_detect(x[["..."]], stringr::fixed(strings))
  expect(all(found),paste0("Params not inherited: ",paste0(names(params)[!found],collapse=",")))
}

test_that("inheritDotParams inherits `...` parameters from parent", {
  out <- roc_proc_text(rd_roclet(), "
    #' Foo
    #'
    #' @param x x
    #' @param y y
    #' @param \\dots foo
    foo <- function(x, y, ...) {}

    #' Bar
    #'
    #' @inheritAllDotParams foo
    bar <- function(...) {}
  ")[[2]]

  # I expect to see here the foo dot params documented

  .expect_inherited(
    out$get_value("param"),
    c(x = "x", y="y", "\\dots" = "foo")
  )

})

test_that("inheritDotParams inherits `...` parameters from mulitple parents including from dplyr", {
  out <- roc_proc_text(rd_roclet(), "
    #' Bar
    #'
    #' @param y ybar
    #' @param \\dots not used
    bar <- function(y, ...) {}

    #' Foo
    #'
    #' @param .messages like in dtrackr
    #' @inheritParams dplyr::mutate
    #' @inheritAllDotParams dplyr::mutate
    #' @inheritAllDotParams bar
    foo <- function(.data, ..., .messages) {}
  ")[[2]]

  # I expect to see here the foo dot params documented

  .expect_inherited(
    out$get_value("param"),
    c(y="ybar")
  )

  lapply(c("Name-value pairs.", "\\.by", "\\.keep"), function(.x) {
    expect(
      stringr::str_detect(out$get_value("param")[["..."]],.x),
      paste0("could not find documentation string: ",.x)
    )
  })


})

## fix 1671 ----
# with fix 1670 this cannot really happen any more, as for a `...`
# to be passed to parent it is an error to not have a `...`
# in the parent to consume unexpected parameters. In a way this
# should throw an error
test_that("inheritDotParams does nothing if nothing matched", {
  out <- roc_proc_text(rd_roclet(), "
    #' Foo
    #'
    #' @param x xfoo
    #' @param y yfoo
    foo <- function(x, y) {}

    #' Bar
    #'
    #' @param x xbar
    #' @param y ybar
    #' @inheritAllDotParams foo
    bar <- function(x,y,...) {}
  ")[[2]]

  # I expect to see here the bar params documented

  expect_equal(
    out$get_value("param"),
    c(x="xbar", y="ybar")
  )

  # No inherited section and specifically no empty code block
  # that triggers CRAN NOTE.
  expect_false(
    any(stringr::str_detect(
      format(out$get_section("param")),
      stringr::fixed("\\item{\\code{}}{}"))
    )
  )


})


test_that("inheritDotParams does nothing if dots documented", {
  out <- roc_proc_text(rd_roclet(), "
    #' Foo
    #'
    #' @param x xfoo
    #' @param y yfoo
    #' @param \\dots dotsfoo
    foo <- function(x, y, ...) {}

    #' Bar
    #'
    #' @param x xbar
    #' @param y ybar
    #' @param \\dots dotsbar
    #' @inheritAllDotParams foo
    bar <- function(x,y,...) {}
  ")[[2]]

  # I expect to see here the bar params documented and no foo params

  expect_equal(
    out$get_value("param"),
    c(x="xbar", y="ybar", "\\dots"="dotsbar")
  )

})


test_that("inheritAllDotParams inheritance is transmitted (mostly)", {
  out <- roc_proc_text(rd_roclet(), "
    #' Foo
    #'
    #' @param x xfoo
    #' @param \\dots dotsfoo
    foo <- function(x,...) {}

    #' Bar
    #'
    #' @param y ybar
    #' @inheritAllDotParams foo
    bar <- function(y,...) {}

    #' Baz
    #'
    #' @param z zbaz
    #' @inheritAllDotParams bar
    baz <- function(z,...) {}
  ")[[3]]

  # This can be broken by placing the functions out of natural order
  # so that `baz` is defined before `bar`.


  expect_equal(
    out$get_value("param")[1],
    c(z="zbaz")
  )

  .expect_inherited(
    out$get_value("param"),
    c(x="xfoo", y="ybar", "\\dots"="dotsfoo")
  )

})


