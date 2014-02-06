context("rdComplete")

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
