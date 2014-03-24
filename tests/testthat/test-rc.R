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

test_that("trim_docstring handles indentation correctly", {
  expect_equal(trim_docstring("a\n  b\n  c"), "a\nb\nc")
  expect_equal(trim_docstring("a\nb\nc"), "a\nb\nc")
  expect_equal(trim_docstring("a\n  b\n   c"), "a\nb\n c")
  expect_equal(trim_docstring("   a\n  b\n   c"), "a\nb\n c")
})

# Method documentation ---------------------------------------------------------

env <- pkg_env()

A <- setRefClass("A", methods = list(
  f = function() {
    "This function has a docstring"
    1
  },
  g = function() {
    "This function doesn't"
  }
), where = env)

B <- setRefClass("B", contains = "A", methods = list(
  f1 = function() {
    "This function has a docstring"
    1
  },
  g1 = function() {
    "This function doesn't"
  }
), where = env)

classA <- getClass("A", where = env)
classB <- getClass("B", where = env)

test_that("rc_methods only lists methods belong to class (not parents)", {
  expect_equal(length(rc_methods(classA)), 2)
  expect_equal(length(rc_methods(classB)), 2)
})

test_that("RC methods included included in own section", {
  out <- roc_proc_text(rd_roclet(), "
    #' Class ABC
    setRefClass('ABC', methods = list(
      f = function() {
      'This function has a docstring'
      1
      }
    ))
  ")[[1]]

  methods <- get_tag(out, "rcmethods")$values
  expect_equal(names(methods), "f()")
  expect_match(methods[[1]], "This function has a docstring")
})

removeClass("B", where = env)
removeClass("A", where = env)
