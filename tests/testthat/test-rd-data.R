context("Rd: data")

test_that("can document eager data", {
  skip_if_not_installed("devtools")

  test_pkg <- temp_copy_pkg('testEagerData')
  on.exit(unlink(test_pkg, recursive = TRUE))

  expect_output(devtools::document(test_pkg), "a[.]Rd")
  expect_true(file.exists(file.path(test_pkg, "man", "a.Rd")))
})

test_that("can document lazy data", {
  skip_if_not_installed("devtools")

  test_pkg <- temp_copy_pkg('testLazyData')
  on.exit(unlink(test_pkg, recursive = TRUE))

  expect_output(devtools::document(test_pkg), "a[.]Rd")
  expect_true(file.exists(file.path(test_pkg, "man", "a.Rd")))
})
