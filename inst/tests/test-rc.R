context("rc")

# Docstrings -------------------------------------------------------------------

test_that("base functions don't have docstrings", {
  expect_equal(docstring(`[`), NULL)
  expect_equal(docstring(mean), NULL)
})

test_that("function return string doesn't have docstring", {
  expect_equal(docstring(function() "a"), NULL)
  expect_equal(docstring(function() {"a"}), NULL)
})

test_that("first string in function is docstring", {
  expect_equal(docstring(function() {"a"; 1}), "a")
})

# Method documentation ---------------------------------------------------------


A <- setRefClass("A", methods = list(
  f = function() {
    "This function has a docstring"
    1
  },
  g = function() {
    "This function doesn't"
  }
))

B <- setRefClass("B", contains = "A", methods = list(
  f1 = function() {
    "This function has a docstring"
    1
  },
  g1 = function() {
    "This function doesn't"
  }
))

test_that("rc_methods lists all methods", {
  expect_equal(length(rc_methods(A)), 2)
  expect_equal(length(rc_methods(B)), 4)
})
