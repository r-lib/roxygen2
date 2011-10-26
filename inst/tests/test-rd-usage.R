context("Rd - usage")
roc <- rd_roclet()

test_that("usage captured from formals", {
  out <- roc_proc_text(roc, "
    #' Title.
    a <- function(a=1) {}")[[1]]
  expect_equal(get_tag(out, "usage")$values, "a(a\u{A0}=\u{A0}1)")
})

test_that("usage correct for modification functions", {
  out <- roc_proc_text(roc, "
    #' Title.
    `foo<-` <- function(a=1) {}")[[1]]
  
  expect_equal(get_tag(out, "usage")$values, "foo(a\u{A0}=\u{A0}1) <- value")
})

test_that("usage correct for functions with no arguments", {
  out <- roc_proc_text(roc, "
      #' Function without parameters
      f <- function() 1")[[1]]
  
  expect_equal(get_tag(out, "usage")$values, "f()")
})

test_that("% is escaped in usage", {
  out <- roc_proc_text(roc, "
    #' Title.
    a <- function(a='%') {}")[[1]]
  expect_equal(get_tag(out, "usage")$values, "a(a\u{A0}=\u{A0}\"\\%\")")
})

test_that("long usages protected from incorrect breakage", {
  out <- roc_proc_text(roc, "
      #' Function long usage
      f <- function(a = '                             a', 
                    b = '                             b', 
                    c = '                             c', 
                    d = '                             ') 1")[[1]]
  
  usage <- format(get_tag(out, "usage"))
  expect_equal(str_count(usage, "\n"), 6)
})

test_that("@usage overrides default", {
  out <- roc_proc_text(roc, "
    #' @usage a(a=2)
    a <- function(a=1) {}")[[1]]
    expect_equal(get_tag(out, "usage")$values, "a(a=2)")
})

test_that("quoted topics have usage statements", {
  out <- roc_proc_text(roc, "
    #' Title.
    \"f\" <- function(a = 1, b = 2) {}")[[1]]
  
  expect_equal(get_tag(out, "usage")$values, "f(a = 1, b = 2)")
  
})
