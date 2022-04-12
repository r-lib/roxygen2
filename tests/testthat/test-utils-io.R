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
  expect_equal(readChar(win_nl, 100), "baz\r\n")

  write_lines("baz", unix_nl)
  expect_equal(readChar(unix_nl, 100), "baz\n")

  write_lines("baz", non_existent_file)
  expect_equal(readChar(non_existent_file, 100), "baz\n")

  write_lines("baz", empty_file)
  expect_equal(readChar(empty_file, 100), "baz\n")
})

test_that("write_lines writes unix-style line endings.", {
  path <- test_path("escapes.Rd")
  # skip if checked on windows with autocrlf = true
  skip_if(detect_line_ending(path) == "\r\n")

  temp <- withr::local_tempfile()
  write_lines(read_lines(path), temp)

  old_binary <- readBin(path, "raw", n = file.info(path)$size)
  new_binary <- readBin(temp, "raw", n = file.info(temp)$size)
  expect_equal(new_binary, old_binary)
})
