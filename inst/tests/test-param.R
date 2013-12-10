context("Param")
roc <- rd_roclet()

test_that("@param documents arguments", {
  out <- roc_proc_text(roc, "
    #' @param a an incipit letter
    #' @param z a terminal letter
    a <- function(a=1, z=2) {}")[[1]]
  
  args <- get_tag(out, "arguments")$values
  expect_equivalent(args["a"], "an incipit letter")
  expect_equivalent(args["z"], "a terminal letter")
})

test_that("multiple @inheritParam tags gathers all params", {
  out <- roc_proc_text(roc, "
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

  params <- get_tag(out[["c.Rd"]], "arguments")$values
  expect_equal(length(params), 2)

  expect_equal(params[["x"]], "X")
  expect_equal(params[["y"]], "Y")
})

test_that("multiple @inheritParam inherits from existing topics", {
  out <- roc_proc_text(roc, "
    #' My mean
    #'
    #' @inheritParams base::mean
    mymean <- function(x, trim) {}")[[1]]
  params <- get_tag(out, "arguments")$values
  expect_equal(length(params), 2)
  expect_equal(sort(names(params)), c("trim", "x"))
})

test_that("@inheritParam understands compound docs", {
  out <- roc_proc_text(roc, "
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
  params <- get_tag(out, "arguments")$values
  expect_equal(params, c(x = "y", y = "y"))
})


test_that("methods inherit from generics by default", {
  out <- roc_proc_text(roc, "
    #' Blah.
    #' 
    #' @param object blah blah blah
    setGeneric('blah', function(object){
      standardGeneric('blah')
    })
    
    #' Title.
    setMethod('blah', 'numeric', function(object){ show(NA) })
    ")[[2]]
  
  expect_equal(names(get_tag(out, "arguments")$values), c("object"))
})

test_that("data objects don't get params", {
  out <- roc_proc_text(roc, "
    #' @rdname xy
    x <- 'x'
  ")[[1]]
  expect_equal(get_tag(out, "arguments"), NULL)
  
})
