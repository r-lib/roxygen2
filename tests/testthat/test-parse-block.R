context("parse_block")

test_that("parses into tag and value", {
  x <- parse_block("#' @xyz abc")
  expect_equal(length(x), 1)

  expect_equal(x[[1]]$tag, "xyz")
  expect_equal(x[[1]]$val, "abc\n")
})

test_that("description block gets empty tag", {
  x <- parse_block("#' abc")
  expect_equal(length(x), 1)

  expect_equal(x[[1]]$tag, "")
  expect_equal(x[[1]]$val, "abc\n")
})

test_that("multi line tags collapsed into one", {
  x <- parse_block(c(
    "#' @tag abc",
    "#'   def"
  ))
  expect_equal(length(x), 1)
  expect_equal(x[[1]]$val, "abc\ndef\n")
})

test_that("description block gets empty tag when followed by tag", {
  x <- parse_block(c(
    "#' abc",
    "#' @xyz abc"
  ))
  expect_equal(length(x), 2)

  expect_equal(x[[1]]$tag, "")
  expect_equal(x[[1]]$val, "abc\n")

  expect_equal(x[[2]]$tag, "xyz")
  expect_equal(x[[2]]$val, "abc\n")
})

test_that("whitespace is ignored", {
  ref <- parse_block("#' abc")

  expect_equal(parse_block("   #' abc"), ref)
  expect_equal(parse_block("#'    abc"), ref)
})

test_that("@@ becomes @", {
  expect_equal(parse_block("#' @tag @@")[[1]]$val, "@\n")
})
