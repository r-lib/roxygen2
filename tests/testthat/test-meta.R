context("Meta")

test_that("evaluation of 'man/roxygen/meta.R' must return a list", {

  tmpdir <- tempfile("roxygen-meta-")
  on.exit(unlink(tmpdir), add = TRUE)

  metafile <- file.path(tmpdir, "man/roxygen/meta.R")
  dir.create(dirname(metafile), recursive = TRUE)

  # return FALSE when no metafile exists
  expect_false(roxy_meta_load(tmpdir))

  # error when doesn't return a list
  write_lines("42", metafile)
  expect_error(roxy_meta_load(tmpdir))

  # TRUE when a list is returned
  write_lines("list(alpha = 1)", metafile)
  expect_true(roxy_meta_load(tmpdir))

  # can access meta now
  expect_equal(roxy_meta_get("alpha"), 1)

})
