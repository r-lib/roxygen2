test_that("nice_name leaves ok chars unchanged", {
  expect_equal(nice_name("abc"), "abc")
  expect_equal(nice_name("a_b-c.R"), "a_b-c.R")
})

test_that("nice_name protects against invalid characters", {
  expect_equal(nice_name("a<-"), "a-set")
  expect_equal(nice_name("[.a"), "sub-.a")
})

test_that("is_namespaced works as expected", {
  expect_true(is_namespaced("a::b"))
  expect_false(is_namespaced("b::"))
  expect_false(is_namespaced("'::'"))
})

test_that("write_if_different produces informative messages", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "test.R")

  write_lines(made_by("#"), path)
  expect_snapshot(write_if_different(path, "a <- 2"))

  write_lines("a <- 1", path)
  # strip temp path
  expect_snapshot(
    write_if_different(path, "a <- 2"),
    transform = function(x) gsub(dir, "", x, fixed = TRUE)
  )

  path <- file.path(dir, "+.R")
  expect_snapshot(write_if_different(path, "a <- 2"))
})

test_that("write_if_different and end of line", {
  cnt_unix <- c("foo\nbar\nbaz", "foobar")
  cnt_win <- c("foo\r\nbar\r\nbaz", "foobar")
  cnt_mix <- c("foo\nbar\r\nbaz", "foobar")

  tmp <- tempfile("roxy-", fileext = ".Rd")
  on.exit(unlink(tmp), add = TRUE)

  # do not change unix le
  write_lines(cnt_unix, tmp, line_ending = "\n")
  expect_message(write_if_different(tmp, cnt_unix, check = FALSE), NA)
  expect_message(write_if_different(tmp, cnt_win, check = FALSE), NA)
  expect_message(write_if_different(tmp, cnt_mix, check = FALSE), NA)

  # do not change windows le
  write_lines(cnt_win, tmp, line_ending = "\r\n")
  expect_message(write_if_different(tmp, cnt_unix, check = FALSE), NA)
  expect_message(write_if_different(tmp, cnt_win, check = FALSE), NA)
  expect_message(write_if_different(tmp, cnt_mix, check = FALSE), NA)

  # change mixed le to windows
  tmp_win <- tempfile("roxy-", fileext = ".Rd")
  on.exit(unlink(tmp_win), add = TRUE)
  write_lines(cnt_win, tmp_win, line_ending = "\r\n")

  # write_lines changes line endings, so we use writeBin to create a file with mixed
  # line endings
  raw_mix <- charToRaw(paste0(paste0(cnt_mix, collapse = "\r\n"), "\r\n"))
  writeBin(raw_mix, tmp)
  expect_message(write_if_different(tmp, cnt_unix, check = FALSE), "Writing ")
  expect_identical(readBin(tmp, "raw", 100), readBin(tmp_win, "raw", 100))

  writeBin(raw_mix, tmp)
  expect_message(write_if_different(tmp, cnt_win, check = FALSE), "Writing ")
  expect_identical(readBin(tmp, "raw", 100), readBin(tmp_win, "raw", 100))

  writeBin(raw_mix, tmp)
  expect_message(write_if_different(tmp, cnt_mix, check = FALSE), "Writing ")
  expect_identical(readBin(tmp, "raw", 100), readBin(tmp_win, "raw", 100))
})

test_that("write_if_different produces correct command hyperlink", {
  testthat::local_reproducible_output(hyperlinks = TRUE)

  dir <- withr::local_tempdir()
  path <- file.path(dir, "test.R")

  write_lines(made_by("#"), path)
  expect_snapshot(write_if_different(
    path,
    "a <- 2",
    command = "rlang::inform('hi')"
  ))
})
