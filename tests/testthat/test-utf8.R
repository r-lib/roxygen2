context("nonASCII")

test_that("can generate nonASCII document", {
  test_pkg <- temp_copy_pkg(test_path('testNonASCII'))
  on.exit(unlink(test_pkg, recursive = TRUE), add = TRUE)

  expect_output(roxygenise(test_pkg, roclets = "rd"), "printChineseMsg[.]Rd")

  rd_path <- file.path(test_pkg, "man", "printChineseMsg.Rd")
  expect_true(file.exists(rd_path))
  rd <- read_lines(rd_path)

  expect_true(any(grepl("\u6211\u7231\u4e2d\u6587", rd)))
  expect_true(any(grepl("\u4e2d\u6587\u6ce8\u91ca", rd)))

  # Shouldn't change again
  expect_output(roxygenise(test_pkg, roclets = "rd"), NA)
})


test_that("unicode escapes are ok", {
  test_pkg <- temp_copy_pkg(test_path('testUtf8Escape'))
  on.exit(unlink(test_pkg, recursive = TRUE), add = TRUE)

  expect_output(roxygenise(test_pkg, roclets = "rd"), "a[.]Rd")

  rd_path <- file.path(test_pkg, "man", "a.Rd")
  expect_true(file.exists(rd_path))
  rd <- read_lines(rd_path)

  expect_true(any(grepl("7\u00b0C", rd)))

  # Shouldn't change again
  expect_output(roxygenise(test_pkg, roclets = "rd"), NA)
})

test_that("write_lines writes unix-style line endings.", {
  path <- test_path("escapes.Rd")
  temp_filename <- tempfile()
  old_binary <- readBin(path, "raw", n = file.info(path)$size)
  old_text <- read_lines(path)
  write_lines(old_text, temp_filename)
  on.exit(unlink(temp_filename), add = TRUE)
  new_binary <- readBin(temp_filename, "raw", n = file.info(temp_filename)$size)
  expect_identical(new_binary, old_binary)
})
