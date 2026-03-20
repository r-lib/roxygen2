test_that("empty string passes through", {
  result <- escape_rd_for_md_c("")
  expect_equal(result$text, "")
  expect_equal(result$id, "")
  expect_equal(as.character(result$tags), character())
})

test_that("plain text is only double-escaped", {
  result <- escape_rd_for_md_c("plain text")
  expect_equal(result$text, "plain text")
  expect_equal(as.character(result$tags), character())
})

test_that("backslashes are doubled", {
  result <- escape_rd_for_md_c(r"(a \ b)")
  expect_equal(result$text, r"(a \\ b)")
})

test_that("\\[ and \\] are not doubled", {
  result <- escape_rd_for_md_c(r"(\[ not a link \])")
  expect_equal(result$text, r"(\[ not a link \])")
})

test_that("non-fragile tags are double-escaped, not captured", {
  result <- escape_rd_for_md_c(r"(\emph{text})")
  expect_equal(result$text, r"(\\emph{text})")
  expect_equal(as.character(result$tags), character())
})

test_that("fragile tag with one argument", {
  result <- escape_rd_for_md_c(r"(\code{foo()})")
  expect_match(result$text, paste0(result$id, "-1-"))
  expect_equal(as.character(result$tags), r"(\code{foo()})")
})

test_that("fragile tag with no arguments", {
  result <- escape_rd_for_md_c(r"(\url rest)")
  expect_equal(as.character(result$tags), r"(\url)")
  expect_match(result$text, "rest")
})

test_that("fragile tag with multiple argument groups", {
  # \link{fun} captures the {fun} argument
  result <- escape_rd_for_md_c(r"(\link{fun})")
  expect_equal(as.character(result$tags), r"(\link{fun})")

  # \link[pkg]{fun} — the [ stops argument scanning, so only \link is captured
  result2 <- escape_rd_for_md_c(r"(\link[pkg]{fun})")
  expect_equal(as.character(result2$tags), r"(\link)")
})

test_that("multiple fragile tags", {
  result <- escape_rd_for_md_c(r"(See \code{foo()} and \link{bar})")
  expect_equal(length(result$tags), 2)
  expect_equal(as.character(result$tags), c(r"(\code{foo()})", r"(\link{bar})"))
})

test_that("nested fragile tags captured as one", {
  result <- escape_rd_for_md_c(r"(\code{a \link{b} c})")
  expect_equal(length(result$tags), 1)
  expect_equal(as.character(result$tags), r"(\code{a \link{b} c})")
})

test_that("adjacent fragile tags", {
  result <- escape_rd_for_md_c(r"(\code{a}\link{b})")
  expect_equal(length(result$tags), 2)
  expect_equal(as.character(result$tags), c(r"(\code{a})", r"(\link{b})"))
})

test_that("Rd escapes inside tags", {
  result <- escape_rd_for_md_c(r"(\code{a \{ b})")
  expect_equal(as.character(result$tags), r"(\code{a \{ b})")
})

test_that("% comments inside tags", {
  result <- escape_rd_for_md_c("\\code{a % comment\nb}")
  expect_equal(as.character(result$tags), "\\code{a % comment\nb}")
})

test_that("text around fragile tags is double-escaped", {
  result <- escape_rd_for_md_c(r"(a \ \code{x} \ b)")
  expect_equal(as.character(result$tags), r"(\code{x})")
  # The backslashes outside the tag should be doubled
  expect_true(grepl("a \\\\ ", result$text, fixed = TRUE))
  expect_true(grepl(" \\\\ b", result$text, fixed = TRUE))
})

test_that("round-trip escape/unescape preserves Rd tags", {
  cases <- c(
    r"(See \code{foo()} and \link{bar} for details.)",
    r"(\code{a \link{b} c})",
    ""
  )
  for (text in cases) {
    escaped <- escape_rd_for_md(text)
    result <- unescape_rd_for_md(escaped, escaped)
    expect_equal(result, text, info = text)
  }
})

test_that("unescape_rd_for_md_c restores placeholders", {
  id <- "TESTID"
  rd_text <- paste0("See ", id, "-1- for details")
  result <- unescape_rd_for_md_c(rd_text, id, r"(\code{foo()})")
  expect_equal(result, r"(See \code{foo()} for details)")
})
