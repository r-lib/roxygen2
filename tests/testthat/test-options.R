test_that("merged options from can load options from DESCRIPTION", {
  opts <- load_options(test_path("test-options"))
  expect_equal(opts$old_usage, TRUE) # from DESCRIPTION
  expect_equal(opts$markdown, TRUE) # from meta.R
})

test_that("warns on invalid meta.R files", {
  expect_warning(
    load_options_meta(test_path("test-options"), "meta-error.R"),
    "Failed to source"
  )

  expect_warning(
    load_options_meta(test_path("test-options"), "meta-character.R"),
    "yield a named list"
  )
})
