test_that("detect_line_ending works", {
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

  expect_equal(detect_line_ending(win_nl), "\r\n")
  expect_equal(detect_line_ending(unix_nl), "\n")
  expect_equal(detect_line_ending(non_existent_file), "\n")
  expect_equal(detect_line_ending(empty_file), "\n")
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
  win_n1_conn <- file(win_nl, "rb")
  expect_equal(readChar(win_n1_conn, 100), "baz\r\n")
  close(win_n1_conn)

  write_lines("baz", unix_nl)
  unix_nl_conn <- file(unix_nl, "rb")
  expect_equal(readChar(unix_nl_conn, 100), "baz\n")
  close(unix_nl_conn)

  write_lines("baz", non_existent_file)
  non_existent_file_conn <- file(non_existent_file, "rb")
  expect_equal(readChar(non_existent_file_conn, 100), "baz\n")
  close(non_existent_file_conn)

  write_lines("baz", empty_file)
  empty_file_conn <- file(empty_file, "rb")
  expect_equal(readChar(empty_file_conn, 100), "baz\n")
  close(empty_file_conn)
})
