# rdComplete ---------------------------------------------------------------

test_that("braces must balance", {
  expect_true(rdComplete("{}", is_code = FALSE))
  expect_true(rdComplete("{{}}", is_code = FALSE))

  expect_false(rdComplete("{", is_code = FALSE))
  expect_false(rdComplete("}", is_code = FALSE))
})

test_that("can't end with escape", {
  expect_false(rdComplete("\\", is_code = FALSE))
})

test_that("escaped brackets are ignored", {
  expect_true(rdComplete("\\{", is_code = FALSE))
  expect_true(rdComplete("\\}", is_code = FALSE))
  expect_false(rdComplete("{\\}", is_code = FALSE))
})

test_that("brackets in comments are ignored", {
  expect_true(rdComplete("% {", is_code = FALSE))
  expect_true(rdComplete("% }", is_code = FALSE))
})

test_that("R comments don't close latex-like tags", {
  expect_true(rdComplete(r"(A comment \code{#}.)", is_code = FALSE))
})

test_that("newline ends comment", {
  expect_false(rdComplete("%\n{", is_code = FALSE))
})

test_that("escape disables comment", {
  expect_false(rdComplete(r"(\%{)", is_code = FALSE))
})

test_that("strings must be closed in code", {
  expect_false(rdComplete("'", is_code = TRUE))
  expect_false(rdComplete('"', is_code = TRUE))
})

test_that("strings respect escapes", {
  expect_false(rdComplete("'\\'", is_code = TRUE)) # '\'
  expect_true(rdComplete("'\\''", is_code = TRUE)) # '\''
})

test_that("braces in strings don't need to match in code", {
  expect_true(rdComplete("'{{'", is_code = TRUE))
})

test_that("strings in code comments don't need to be closed", {
  expect_true(rdComplete("# '", is_code = TRUE))
})

test_that("braces in code must match", {
  expect_false(rdComplete("# {", is_code = TRUE))
  expect_true(rdComplete("# {}", is_code = TRUE))
})

# findEndOfTag -------------------------------------------------------------

test_that("handles simple cases", {
  expect_equal(findEndOfTag("{abc}", is_code = FALSE), 4)
  expect_equal(findEndOfTag("{abc} rest", is_code = FALSE), 4)
  expect_equal(findEndOfTag("{a}{b} rest", is_code = FALSE), 5)
  expect_equal(findEndOfTag("{a{b}c} rest", is_code = FALSE), 6)
})

test_that("returns -1 for incomplete input", {
  expect_equal(findEndOfTag("{abc", is_code = FALSE), -1)
})
