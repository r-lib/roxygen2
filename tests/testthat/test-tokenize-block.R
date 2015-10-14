context("tokenize_block")

test_that("parses into tag and value", {
  x <- tokenise_preref("#' @xyz abc")
  expect_equal(length(x), 1)

  expect_equal(x[[1]]$tag, "xyz")
  expect_equal(x[[1]]$val, "abc")
})

test_that("description block gets empty tag", {
  x <- tokenise_preref("#' abc")
  expect_equal(length(x), 1)

  expect_equal(x[[1]]$tag, "")
  expect_equal(x[[1]]$val, "abc")
})

test_that("multi line tags collapsed into one", {
  x <- tokenise_preref(c(
    "#' @tag abc",
    "#'   def"
  ))
  expect_equal(length(x), 1)
  expect_equal(x[[1]]$val, "abc\n  def")
})

test_that("description block gets empty tag when followed by tag", {
  x <- tokenise_preref(c(
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
  ref <- tokenise_preref("#' abc")

  expect_equal(tokenise_preref("   #' abc"), ref)
})

test_that("need one or more #", {
  ref <- tokenise_preref("#' abc")

  expect_equal(tokenise_preref("##' abc"), ref)
  expect_equal(tokenise_preref("###' abc"), ref)
})

test_that("@@ becomes @", {
  expect_equal(tokenise_preref("#' @tag @@")[[1]]$val, "@")
})
