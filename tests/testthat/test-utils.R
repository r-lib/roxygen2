# nice_names --------------------------------------------------------------

test_that("nice_name leaves ok chars unchanged", {
  expect_equal(nice_name("abc"), "abc")
  expect_equal(nice_name("a_b-c.R"), "a_b-c.R")
})

test_that("nice_name protects against invalid characters", {
  expect_equal(nice_name("a<-"), "a-set")
  expect_equal(nice_name("[.a"), "sub-.a")
})

test_that("same_contents with Unicode file names on Windows", {
  skip_on_cran()
  if (.Platform$OS.type != "windows") skip("Only on windows")
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp)
  path <- file.path(tmp, "tricky-\u0151")
  contents <- "foobar"
  # Need to write raw, otherwise it will be \r\n on Windows
  writeBin(charToRaw(paste0(contents, "\n")), con = path)
  expect_silent(res <- same_contents(path, contents))
  expect_true(res)
})
