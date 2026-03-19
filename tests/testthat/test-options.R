test_that("can load options from old Roxygen field", {
  opts <- load_options(test_path("test-options"))
  expect_equal(opts$old_usage, TRUE) # from DESCRIPTION
  expect_equal(opts$markdown, TRUE) # from meta.R
})

test_that("can load options from Config/roxygen2/ fields", {
  opts <- load_options(test_path("test-options-config"))
  expect_equal(opts$old_usage, TRUE)
  expect_equal(opts$packages, c("mypkg1", "mypkg2"))
})

test_that("Config/roxygen2/ fields take priority over Roxygen field", {
  opts <- load_options(test_path("test-options-both"))
  expect_equal(opts$old_usage, TRUE) # from Roxygen field

  expect_equal(opts$markdown, FALSE) # Config overrides Roxygen
})

test_that("parse_config_value handles scalars and vectors", {
  expect_equal(parse_config_value("TRUE"), TRUE)
  expect_equal(parse_config_value("FALSE"), FALSE)

  expect_equal(parse_config_value("installed"), "installed")
  expect_equal(parse_config_value("a, b, c"), c("a", "b", "c"))
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
