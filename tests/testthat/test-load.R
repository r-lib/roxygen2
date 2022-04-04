test_that("load_installed retrieves installed package", {
  skip_if_not(file.exists(test_path("../../DESCRIPTION")))
  env <- load_installed(test_path("../.."))
  expect_identical(env, asNamespace("roxygen2"))
})

test_that("can load simple package with load_pkgload()", {
  suppressMessages(env <- load_pkgload(test_path("testRbuildignore")))
  expect_equal(env_get(env, "a"), 1)
})

test_that("can load simple package with load_source()", {
  env <- load_source(test_path("testRbuildignore"))
  expect_equal(env_get(env, "a"), 1)
})

# find_load_strategy ------------------------------------------------------

test_that("function returned as is", {
  expect_equal(find_load_strategy(load_installed), load_installed)
})

test_that("look up string", {
  expect_equal(find_load_strategy("installed"), load_installed)
  expect_equal(find_load_strategy("pkgload"), load_pkgload)
  expect_equal(find_load_strategy("source"), load_source)
  expect_error(find_load_strategy("blahblahb"), "Unknown value")
})

test_that("NULL uses option", {
  expect_equal(find_load_strategy(NULL, "installed"), load_installed)
})

test_that("informative errors for bad inputs", {
  expect_error(find_load_strategy(1), "string or function")
  expect_error(find_load_strategy(NULL, list()), "string")
})
