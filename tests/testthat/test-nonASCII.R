context("nonASCII")

test_that("can generate nonASCII document", {
  test_pkg <- temp_copy_pkg('testNonASCII')
  on.exit(unlink(test_pkg, recursive = TRUE), add = TRUE)

  expect_output(roxygenise(test_pkg, roclets = "rd"), "printChineseMsg[.]Rd")
  expect_true(file.exists(file.path(test_pkg, "man", "printChineseMsg.Rd")))

  cnChar <- read_lines_enc(file.path(test_pkg, "man", "printChineseMsg.Rd"))

  # Because the parse in testthat::test don't specify encoding to UTF-8 as well,
  # so we have to use unicode escapes.
  expect_true(any(grepl("\u6211\u7231\u4e2d\u6587", cnChar)))
  expect_true(any(grepl("\u4e2d\u6587\u6ce8\u91ca", cnChar)))

  # No output on second run
  expect_output(roxygenise(test_pkg, roclets = "rd"), NA)
})
