ph <- function(i) paste0("\uE000", i, "\uE001")

test_that("md_tokenize splits text into markdown and Rd tokens", {
  tk <- md_tokenize("a \\code{x *y*} *b* \\emph{*c*}")
  expect_equal(tk$text, paste0("a ", ph(1), " *b* ", ph(2), "{*c*}"))
  expect_equal(tk$tokens, c("\\code{x *y*}", "\\emph"))
  expect_equal(tk$types, c("verbatim", "tag"))
})

test_that("verbatim tags consume all brace groups", {
  expect_equal(md_tokenize("\\ifelse{a}{b}{c} x")$tokens, "\\ifelse{a}{b}{c}")
})

test_that("brace matching follows Rd rules", {
  # escaped braces don't count
  expect_equal(md_tokenize("\\code{a \\} b}")$tokens, "\\code{a \\} b}")
  # braces nest
  expect_equal(md_tokenize("\\code{a {b} c}")$tokens, "\\code{a {b} c}")
  # % comments out the rest of the line, so this group never completes and
  # the tag falls back to a bare name, with the braces left to markdown
  expect_equal(md_tokenize("\\code{a % b}")$tokens, "\\code")
})

test_that("escapes and lone backslashes are tokenized", {
  tk <- md_tokenize("\\% \\[ \\\\ \\")
  expect_equal(tk$tokens, c("\\%", "\\[", "\\\\", "\\"))
  expect_equal(tk$types, c("escape", "escape", "escape", "backslash"))
})

test_that("a backslash never consumes a backtick", {
  tk <- md_tokenize("`\\`")
  expect_equal(tk$text, paste0("`", ph(1), "`"))
  expect_equal(tk$types, "backslash")
})

test_that("multibyte characters pass through", {
  tk <- md_tokenize("é \\code{café} ü")
  expect_equal(tk$text, paste0("é ", ph(1), " ü"))
  expect_equal(tk$tokens, "\\code{café}")
})

test_that("pre-existing sentinel characters are stripped with a warning", {
  expect_warning(tk <- md_tokenize("a \uE000 b \uE001 c"), "private-use")
  expect_equal(tk$text, "a  b  c")
  expect_equal(tk$tokens, character())
})

test_that("restore_tokens restores according to context", {
  tk <- md_tokenize("\\code{x} \\emph \\% \\[ \\] \\")
  state <- as.environment(tk)

  expect_equal(
    restore_tokens(tk$text, state, "text"),
    "\\code{x} \\emph \\% [ ] \\"
  )
  expect_equal(
    restore_tokens(tk$text, state, "verb"),
    "\\code{x} \\\\emph \\\\\\% \\\\[ \\\\] \\\\"
  )
  expect_equal(
    restore_tokens(tk$text, state, "raw"),
    "\\code{x} \\emph \\% \\[ \\] \\"
  )
})

test_that("restore_tokens leaves token-free text alone", {
  state <- as.environment(list(tokens = character(), types = character()))
  expect_equal(restore_tokens("plain *text*", state, "text"), "plain *text*")
})
