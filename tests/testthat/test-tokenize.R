# tokenise_block ----------------------------------------------------------

test_that("parses into tag and value", {
  x <- tokenise_block("#' @xyz abc")
  expect_equal(length(x), 1)

  expect_equal(x[[1]]$tag, "xyz")
  expect_equal(x[[1]]$raw, "abc")
})

test_that("description block gets empty tag", {
  x <- tokenise_block("#' abc")
  expect_equal(length(x), 1)

  expect_equal(x[[1]]$tag, "")
  expect_equal(x[[1]]$raw, "abc")
})

test_that("multi line tags collapsed into one", {
  x <- tokenise_block(c(
    "#' @tag abc",
    "#'   def"
  ))
  expect_equal(length(x), 1)
  expect_equal(x[[1]]$raw, "abc\n  def")
})

test_that("description block gets empty tag when followed by tag", {
  x <- tokenise_block(c(
    "#' abc",
    "#' @xyz abc"
  ))
  expect_equal(length(x), 2)

  expect_equal(x[[1]]$tag, "")
  expect_equal(x[[1]]$raw, "abc")

  expect_equal(x[[2]]$tag, "xyz")
  expect_equal(x[[2]]$raw, "abc")
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
  expect_equal(tokenise_block("#' @tag @@")[[1]]$raw, "@")
})

# Inline comments ---------------------------------------------------------

test_that("Inline comments are supported", {
  out <- roc_proc_text(rd_roclet(), "
    #' Description
    a <- function(x) {
      #' @param x an integer
      stopifnot(is.integer(x))
    }")[[1]]
  expect_equal(get_tag(out, "param")$values, c(x = "an integer"))
})

test_that("Inline comments just before the closing brace are allowed", {
  out <- roc_proc_text(rd_roclet(), "
    #' Description
    a <- function(x) {
      #' @param x an integer
      stopifnot(is.integer(x))

      #' @seealso somewhere
    }")[[1]]
  expect_equal(get_tag(out, "seealso")$values, "somewhere")
})

test_that("Inline comments do not extend past the closing brace", {
  out <- roc_proc_text(rd_roclet(), "
    #' Description
    a <- function(x) {
      #' @param x an integer
      stopifnot(is.integer(x))
    }; #' @seealso somewhere")[[1]]
  expect_null(get_tag(out, "seealso"))
})

