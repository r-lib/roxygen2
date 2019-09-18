test_that("load_installed retrieves installed package", {
  env <- load_installed(test_path("../.."))
  expect_identical(env, asNamespace("roxygen2"))
})

test_that("can load simple package with load_pkgload()", {
  env <- load_pkgload(test_path("testRbuildignore"))
  expect_equal(env_get(env, "a"), 1)
})

test_that("can load simple package with load_source()", {
  env <- load_source(test_path("testRbuildignore"))
  expect_equal(env_get(env, "a"), 1)
})
