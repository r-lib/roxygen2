context("nonASCII")

test_that("can generate nonASCII document", {
  test_pkg <- temp_copy_pkg('testNonASCII')
  on.exit(unlink(test_pkg, recursive = TRUE))

  expect_output(roxygenize(test_pkg), "printChineseMsg[.]Rd")
  expect_true(file.exists(file.path(test_pkg, "man", "printChineseMsg.Rd")))

  cnChar <- readLines(file.path(test_pkg, "man", "printChineseMsg.Rd"), encoding = "UTF-8")
  expect_true(any(grepl("我爱中文", cnChar)) && any(grepl("中文注释", cnChar)))

})
