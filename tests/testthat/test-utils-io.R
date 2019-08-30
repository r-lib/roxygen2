test_that("has_windows_le works", {
  # write files with various newlines
  win_nl <- tempfile()
  win_nl_con <- file(win_nl, "wb")

  unix_nl <- tempfile()
  unix_nl_con <- file(unix_nl, "wb")

  non_existent_file <- tempfile()

  empty_file <- tempfile()
  file.create(empty_file)

  on.exit({
    unlink(c(win_nl, unix_nl, empty_file))
  })

  base::writeLines(c("foo", "bar"), win_nl_con, sep = "\r\n")
  close(win_nl_con)

  base::writeLines(c("foo", "bar"), unix_nl_con, sep = "\n")
  close(unix_nl_con)

  expect_true(has_windows_le(win_nl))
  expect_false(has_windows_le(unix_nl))
  expect_false(has_windows_le(non_existent_file))
  expect_false(has_windows_le(empty_file))
})

test_that("write_lines writes windows newlines for files with windows newlines, and unix newlines otherwise", {
  # write files with various newlines
  win_nl <- tempfile()
  win_nl_con <- file(win_nl, "wb")

  unix_nl <- tempfile()
  unix_nl_con <- file(unix_nl, "wb")

  non_existent_file <- tempfile()

  empty_file <- tempfile()
  file.create(empty_file)

  on.exit({
    unlink(c(win_nl, unix_nl, empty_file, non_existent_file))
  })

  base::writeLines(c("foo", "bar"), win_nl_con, sep = "\r\n")
  close(win_nl_con)

  base::writeLines(c("foo", "bar"), unix_nl_con, sep = "\n")
  close(unix_nl_con)

  write_lines("baz", win_nl)
  expect_equal(readChar(file(win_nl, "rb"), 100), "baz\r\n")

  write_lines("baz", unix_nl)
  expect_equal(readChar(file(unix_nl, "rb"), 100), "baz\n")

  write_lines("baz", non_existent_file)
  expect_equal(readChar(file(non_existent_file, "rb"), 100), "baz\n")

  write_lines("baz", empty_file)
  expect_equal(readChar(file(empty_file, "rb"), 100), "baz\n")
})
