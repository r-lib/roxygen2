context("Parse: block")

test_that("parses into tag and value", {
  x <- tokenise_block("#' @xyz abc")
  expect_equal(length(x), 1)

  expect_equal(x[[1]]$tag, "xyz")
  expect_equal(x[[1]]$val, "abc")
})

test_that("description block gets empty tag", {
  x <- tokenise_block("#' abc")
  expect_equal(length(x), 1)

  expect_equal(x[[1]]$tag, "")
  expect_equal(x[[1]]$val, "abc")
})

test_that("multi line tags collapsed into one", {
  x <- tokenise_block(c(
    "#' @tag abc",
    "#'   def"
  ))
  expect_equal(length(x), 1)
  expect_equal(x[[1]]$val, "abc\n  def")
})

test_that("description block gets empty tag when followed by tag", {
  x <- tokenise_block(c(
    "#' abc",
    "#' @xyz abc"
  ))
  expect_equal(length(x), 2)

  expect_equal(x[[1]]$tag, "")
  expect_equal(x[[1]]$val, "abc")

  expect_equal(x[[2]]$tag, "xyz")
  expect_equal(x[[2]]$val, "abc")
})

test_that("leading whitespace is ignored", {
  ref <- tokenise_block("#' abc")

  expect_equal(tokenise_block("   #' abc"), ref)
})

test_that("need one or more #", {
  ref <- tokenise_block("#' abc")

  expect_equal(tokenise_block("##' abc"), ref)
  expect_equal(tokenise_block("###' abc"), ref)
})

test_that("@@ becomes @", {
  expect_equal(tokenise_block("#' @tag @@")[[1]]$val, "@")
})
