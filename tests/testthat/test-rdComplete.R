context("rdComplete")

# Test low-level behaviour ----------------------------------------------------

test_that("braces must balance", {
  expect_true(rdComplete("{}"))
  expect_true(rdComplete("{{}}"))

  expect_false(rdComplete("{"))
  expect_false(rdComplete("}"))
})

test_that("can't end with escape", {
  expect_false(rdComplete("\\"))
})

test_that("escaped brackets are ignored", {
  expect_true(rdComplete("\\{"))
  expect_true(rdComplete("\\}"))
  expect_false(rdComplete("{\\}"))
})

test_that("brackets in comments are ignored", {
  expect_true(rdComplete("% {"))
  expect_true(rdComplete("% }"))
})

test_that("R comments don't close latex-like tags", {
  expect_true(rdComplete("A comment \\code{#}."))
})

test_that("newline ends comment", {
  expect_false(rdComplete("%\n{"))
})

test_that("escape disables comment", {
  expect_false(rdComplete("\\%{"))
})

test_that("strings must be closed in code", {
  expect_false(rdComplete("'", TRUE))
  expect_false(rdComplete('"', TRUE))
})

test_that("strings respect escapes", {
  expect_false(rdComplete("'\\'", TRUE)) # '\'
  expect_true(rdComplete("'\\''", TRUE)) # '\''
})

test_that("braces in strings don't need to match in code", {
  expect_true(rdComplete("'{{'", TRUE))
})

test_that("strings in code comments don't need to be closed", {
  expect_true(rdComplete("# '", TRUE))
})

test_that("braces in code must match", {
  expect_false(rdComplete("# {", TRUE))
  expect_true(rdComplete("# {}", TRUE))
})




# Test that incomplete Rd is caught in Rd blocks -------------------------------

test_that("incomplete rd in tag raises error", {
  expect_warning(roc_proc_text(rd_roclet(), "
    #' Title
    #' @aliases title{
    x <- 1"), "mismatched braces")
})

test_that("incomplete rd in prequel raises error", {
  expect_warning(roc_proc_text(rd_roclet(), "
    #' Title {
    x <- 1"), "mismatched braces")
})

