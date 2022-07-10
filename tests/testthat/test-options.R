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

test_that("default knitr chunk options are used", {
  local_package_copy(test_path('test-options'))

  callr::r(function() roxygen2::roxygenise(roclets = "rd"))

  expect_snapshot_file(
    "man/foo.Rd",
    transform = function(x) str_replace_all(x, fixed("\r\n"), "\n")
  )
})
