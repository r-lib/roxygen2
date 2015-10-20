context("Param")

test_that("@param documents arguments", {
  out <- roc_proc_text(rd_roclet(), "
    #' @param a an incipit letter
    #' @param z a terminal letter
    a <- function(a=1, z=2) {}")[[1]]

  args <- get_tag(out, "param")$values
  expect_equivalent(args["a"], "an incipit letter")
  expect_equivalent(args["z"], "a terminal letter")
})

test_that("grouped args get spaces", {
  out <- roc_proc_text(rd_roclet(), "
  #' @param a,z Two arguments an incipit letter
  a <- function(a=1, z=2) {}")[[1]]

  args <- get_tag(out, "param")
  expect_match(format(args), "a, z")
})

test_that("multiple @inheritParam tags gathers all params", {
  out <- roc_proc_text(rd_roclet(), "
    #' A.
    #'
    #' @param x X
    a <- function(x) {}


    #' B
    #'
    #' @param y Y
    b <- function(y) {}

    #' C
    #'
    #' @inheritParams a
    #' @inheritParams b
    c <- function(x, y) {}
    ")

  params <- get_tag(out[["c.Rd"]], "param")$values
  expect_equal(length(params), 2)

  expect_equal(params[["x"]], "X")
  expect_equal(params[["y"]], "Y")
})

test_that("multiple @inheritParam inherits from existing topics", {
  out <- roc_proc_text(rd_roclet(), "
    #' My mean
    #'
    #' @inheritParams base::mean
    mymean <- function(x, trim) {}")[[1]]
  params <- get_tag(out, "param")$values
  expect_equal(length(params), 2)
  expect_equal(sort(names(params)), c("trim", "x"))
})

test_that("@inheritParam understands compound docs", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' @param x x
    #' @param y x
    x <- function(x, y) {}

    #' Title
    #'
    #' @inheritParams x
    #' @param x y
    #' @param y y
    y <- function(x, y) {}")[[2]]
  params <- get_tag(out, "param")$values
  expect_equal(params, c(x = "y", y = "y"))
})


test_that("data objects don't get params", {
  out <- roc_proc_text(rd_roclet(), "
    #' @rdname xy
    x <- 'x'
  ")[[1]]
  expect_equal(get_tag(out, "param"), NULL)

})

test_that("find_params parses input", {
  params <- find_params("utils::`?`", NULL, NULL)
  expect_equal(names(params), c("topic", "type"))
})


test_that("argument order, also for incomplete documentation", {
  out <- roc_proc_text(rd_roclet(), "
    #' A.
    #'
    #' @param y Y
    #' @param x X
    a <- function(x, y) {}

    #' B
    #'
    #' @param y Y
    b <- function(x, y) {}

    #' C
    #'
    #' @param x X
    c <- function(x, y) {}

    #' D
    #'
    #' @inheritParams b
    #' @param z Z
    d <- function(x, y, z) {}

    #' E
    #'
    #' @inheritParams c
    #' @param y Y
    e <- function(x, y, z) {}
  ")

  expect_equal(get_tag(out[["a.Rd"]], "param")$values, c(x="X", y="Y"))
  expect_equal(get_tag(out[["b.Rd"]], "param")$values, c(y="Y"))
  expect_equal(get_tag(out[["c.Rd"]], "param")$values, c(x="X"))
  expect_equal(get_tag(out[["d.Rd"]], "param")$values, c(y="Y", z="Z"))
  expect_equal(get_tag(out[["e.Rd"]], "param")$values, c(x="X", y="Y"))
})

test_that("argument order with @inheritParam", {
  out <- roc_proc_text(rd_roclet(), "
    #' A.
    #'
    #' @param x X
    #' @param y Y
    a <- function(x, y) {}

    #' B1
    #'
    #' @param y B
    #' @inheritParams a
    b1 <- function(x, y) {}

    #' B2
    #'
    #' @inheritParams a
    #' @param y B
    b2 <- function(x, y) {}

    #' C1
    #'
    #' @param x C
    #' @inheritParams a
    c1 <- function(x, y) {}

    #' C2
    #'
    #' @inheritParams a
    #' @param x C
    c2<- function(x, y) {}
    ")

  expect_equal(get_tag(out[["b1.Rd"]], "param")$values, c(x="X", y="B"))
  expect_equal(get_tag(out[["b2.Rd"]], "param")$values, c(x="X", y="B"))
  expect_equal(get_tag(out[["c1.Rd"]], "param")$values, c(x="C", y="Y"))
  expect_equal(get_tag(out[["c2.Rd"]], "param")$values, c(x="C", y="Y"))
})

test_that("argument order for multi-parameter documentation", {
  out <- roc_proc_text(rd_roclet(), "
    #' A.
    #'
    #' @param x,y X,Y
    a <- function(x, y) {}

    #' B
    #'
    #' @param y Y
    #' @param x,z X,Z
    #' @param w W
    b <- function(x, y, z, w) {}
    ")

  expect_equal(get_tag(out[["a.Rd"]], "param")$values, c(`x,y`="X,Y"))
  expect_equal(get_tag(out[["b.Rd"]], "param")$values, c(`x,z`="X,Z", y="Y", w="W"))
})

test_that("argument order for multiple usage statements", {
  out <- roc_proc_text(rd_roclet(), "
    #' A.
    #'
    #' @usage a(x, w)
    #' @usage a(x, y)
    #' @usage a(x, z)
    #' @param x X
    #' @param w W
    #' @param y Y
    #' @param z Z
    a <- function(x, y, z, w) {}
    ")

  expect_equal(get_tag(out[["a.Rd"]], "param")$values, c(x="X", y="Y", z="Z", w="W"))
})

test_that("argument order for @rdfile", {
  out <- roc_proc_text(rd_roclet(), "
    #' A
    #'
    #' @param x X
    #' @param y Y
    #' @rdname rd
    a <- function(x, y) {
    }

    #' B
    #'
    #' @export
    #' @rdname rd
    b <- function(y, ...) {
    }
    ")

  expect_equal(get_tag(out[["rd.Rd"]], "param")$values, c(x="X", y="Y"))
})
