test_that("write_lines uses specified line_ending", {
  win_nl <- withr::local_tempfile()
  write_lines("baz", win_nl, line_ending = "\r\n")
  expect_equal(readChar(win_nl, 100), "baz\r\n")

  unix_nl <- withr::local_tempfile()
  write_lines("baz", unix_nl, line_ending = "\n")
  expect_equal(readChar(unix_nl, 100), "baz\n")
})

test_that("detect_line_ending captures existing line ending", {
  win_nl <- withr::local_tempfile()
  write_lines("baz", win_nl, line_ending = "\r\n")
  expect_equal(detect_line_ending(win_nl), "\r\n")

  unix_nl <- withr::local_tempfile()
  write_lines("baz", unix_nl, line_ending = "\n")
  expect_equal(detect_line_ending(unix_nl), "\n")
})

test_that("detect_line_ending defaults to unix", {
  non_existent_file <- withr::local_tempfile()
  expect_equal(detect_line_ending(non_existent_file), "\n")

  empty_file <- withr::local_tempfile()
  file.create(empty_file)
  expect_equal(detect_line_ending(empty_file), "\n")
})

test_that("write_lines preserves existing line ending", {
  win_nl <- withr::local_tempfile()
  write_lines("baz", win_nl, line_ending = "\r\n")
  write_lines("baz", win_nl)
  expect_equal(readChar(win_nl, 100), "baz\r\n")

  unix_nl <- withr::local_tempfile()
  write_lines("baz", unix_nl, line_ending = "\n")
  write_lines("baz", unix_nl)
  expect_equal(readChar(unix_nl, 100), "baz\n")
})
