context("Rd: param")

test_that("@param documents arguments", {
  out <- roc_proc_text(rd_roclet(), "
    #' A
    #' @param a an incipit letter
    #' @param z a terminal letter
    a <- function(a=1, z=2) {}")[[1]]

  args <- get_tag(out, "param")$values
  expect_equivalent(args["a"], "an incipit letter")
  expect_equivalent(args["z"], "a terminal letter")
})

test_that("grouped args get spaces", {
  out <- roc_proc_text(rd_roclet(), "
  #' A
  #' @param a,z Two arguments an incipit letter
  a <- function(a=1, z=2) {}")[[1]]

  args <- get_tag(out, "param")
  expect_match(format(args), "a, z")
})


test_that("data objects don't get params", {
  out <- roc_proc_text(rd_roclet(), "
    #' x
    #' @rdname xy
    x <- 'x'
  ")[[1]]
  expect_equal(get_tag(out, "param"), NULL)

})

test_that("find_params parses input", {
  params <- find_params("utils::`?`", NULL)
  expect_equal(names(params), c("topic", "type"))
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
