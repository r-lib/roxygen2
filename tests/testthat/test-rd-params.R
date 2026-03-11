test_that("@param documents arguments", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A
    #' @param a first
    #' @param z last
    a <- function(a=1, z=2) {}
  "
  )[[1]]

  expect_equal(out$get_value("param"), c(a = "first", z = "last"))
})

test_that("backtick-quoted @param names are parsed correctly (#1696)", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A
    #' @param `arg 1` first
    #' @param `arg 2` last
    a <- function(`arg 1` = 1, `arg 2` = 2) {}
  "
  )[[1]]

  expect_equal(
    out$get_value("param"),
    c("arg 1" = "first", "arg 2" = "last")
  )
})

test_that("grouped args get spaces", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A
    #' @param a,z Two arguments
    a <- function(a=1, z=2) {}
  "
  )[[1]]
  expect_match(out$get_rd("param"), "a, z")
})

test_that("empty @param generates warning", {
  block <- "
    #' A
    #' @param
    #'
    a <- function() {}
  "

  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))
})

test_that("indented @param bullet list is not nested (#1102)", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Foo
    #' @md
    #' @param foo
    #'   * A
    #'   * B
    #'   * C
    a <- function(foo) {}
  "
  )[[1]]

  expect_match(out$get_rd("param"), "\\item A", fixed = TRUE)
  expect_match(out$get_rd("param"), "\\item B", fixed = TRUE)
  expect_match(out$get_rd("param"), "\\item C", fixed = TRUE)
  expect_no_match(out$get_rd("param"), "itemize\\{\n\\\\item B")
})

test_that("data objects don't get params", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' x
    #' @rdname xy
    x <- 'x'
  "
  )[[1]]
  expect_equal(out$get_value("param"), NULL)
})

test_that("arguments ordered by usage", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A
    #'
    #' @param y Y
    #' @param x X
    #' @rdname rd
    a <- function(x, y) {}
  "
  )[[1]]

  expect_named(out$get_value("param"), c("x", "y"))
})

test_that("multiple arguments ordered by first", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' @param y Y
    #' @param x,z X,Z
    #' @param w W
    b <- function(x, y, z, w) {}
  "
  )[[1]]

  expect_named(out$get_value("param"), c("x,z", "y", "w"))
})
