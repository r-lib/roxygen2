context("nonASCII")

test_that("can generate nonASCII document", {
  test_pkg <- temp_copy_pkg('testNonASCII')
  on.exit(unlink(test_pkg, recursive = TRUE))

  expect_output(roxygenize(test_pkg), "printChineseMsg[.]Rd")
  expect_true(file.exists(file.path(test_pkg, "man", "printChineseMsg.Rd")))

  cnChar <- readLines(file.path(test_pkg, "man", "printChineseMsg.Rd"), encoding = "UTF-8")

  # Because the parse in testthat::test don't specify encoding to UTF-8 as well,
  # so we have to use unicode escapes.
  expect_true(any(grepl("\u6211\u7231\u4e2d\u6587", cnChar)))
  expect_true(any(grepl("\u4e2d\u6587\u6ce8\u91ca", cnChar)))
})
