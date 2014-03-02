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

test_that("newline ends comment", {
  expect_false(rdComplete("%\n{"))
})

test_that("escape disables comment", {
  expect_false(rdComplete("\\%{"))
})


# Test that incomplete Rd is caught in Rd blocks -------------------------------

test_that("incomplete rd in tag raises error", {
  expect_error(roc_proc_text(rd_roclet(), "
    #' Title
    #' @aliases title{
    1"), "Mismatched braces")
})

test_that("incomplete rd in prequel raises error", {
  expect_error(roc_proc_text(rd_roclet(), "
    #' Title {
    1"), "Mismatched braces")
})
