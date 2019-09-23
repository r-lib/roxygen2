test_that("@param documents arguments", {
  out <- roc_proc_text(rd_roclet(), "
    #' A
    #' @param a first
    #' @param z last
    a <- function(a=1, z=2) {}
  ")[[1]]

  expect_equal(out$get_value("param"), c(a = "first", z = "last"))
})

test_that("grouped args get spaces", {
  out <- roc_proc_text(rd_roclet(), "
    #' A
    #' @param a,z Two arguments
    a <- function(a=1, z=2) {}
  ")[[1]]
  expect_match(out$get_rd("param"), "a, z")
})

test_that("empty @param generates warning", {
  expect_warning(
    roc_proc_text(rd_roclet(), "
    #' A
    #' @param
    #'
    a <- function() {}"),
    "requires a value"
  )
})

test_that("data objects don't get params", {
  out <- roc_proc_text(rd_roclet(), "
    #' x
    #' @rdname xy
    x <- 'x'
  ")[[1]]
  expect_equal(out$get_value("param"), NULL)
})

test_that("arguments ordered by usage", {
  out <- roc_proc_text(rd_roclet(), "
    #' A
    #'
    #' @param y Y
    #' @param x X
    #' @rdname rd
    a <- function(x, y) {}
  ")[[1]]

  expect_named(out$get_value("param"), c("x", "y"))
})

test_that("multiple arguments ordered by first", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' @param y Y
    #' @param x,z X,Z
    #' @param w W
    b <- function(x, y, z, w) {}
  ")[[1]]

  expect_named(out$get_value("param"), c("x,z", "y", "w"))
})
