test_that("escape_rd_for_md_c handles text without Rd macros", {
  result <- escape_rd_for_md_c("plain text")
  expect_equal(result$text, "plain text")
  expect_equal(result$id, "")
  expect_equal(as.character(result$tags), character())
})

test_that("escape_rd_for_md_c finds and replaces fragile tags", {
  result <- escape_rd_for_md_c(r"(\code{foo()})")
  expect_match(result$text, paste0(result$id, "-1-"))
  expect_equal(as.character(result$tags), r"(\code{foo()})")
})

test_that("escape_rd_for_md_c handles multiple fragile tags", {
  result <- escape_rd_for_md_c(r"(See \code{foo()} and \link{bar})")
  expect_equal(length(result$tags), 2)
  expect_equal(as.character(result$tags), c(r"(\code{foo()})", r"(\link{bar})"))
})

test_that("escape_rd_for_md_c removes embedded fragile tags", {
  result <- escape_rd_for_md_c(r"(\code{a \link{b} c})")
  # Only the outer \code should be kept, not the embedded \link
  expect_equal(length(result$tags), 1)
  expect_equal(as.character(result$tags), r"(\code{a \link{b} c})")
})

test_that("escape_rd_for_md_c double-escapes backslashes", {
  result <- escape_rd_for_md_c(r"(a \ b)")
  expect_equal(result$text, r"(a \\ b)")
})

test_that("escape_rd_for_md_c preserves \\[ and \\]", {
  result <- escape_rd_for_md_c(r"(\[ not a link \])")
  expect_equal(result$text, r"(\[ not a link \])")
})

test_that("unescape_rd_for_md_c restores placeholders", {
  id <- "TESTID"
  rd_text <- paste0("See ", id, "-1- for details")
  result <- unescape_rd_for_md_c(rd_text, id, r"(\code{foo()})")
  expect_equal(result, r"(See \code{foo()} for details)")
})

test_that("round-trip escape/unescape preserves Rd tags", {
  text <- r"(See \code{foo()} and \link{bar} for details.)"
  escaped <- escape_rd_for_md(text)
  result <- unescape_rd_for_md(escaped, escaped)
  expect_equal(result, r"(See \code{foo()} and \link{bar} for details.)")
})
