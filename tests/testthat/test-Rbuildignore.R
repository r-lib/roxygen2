context("Rbuildignore")

test_that("roxygen ignores files with matching pattern in .Rbuildignore", {
  test_pkg <- temp_copy_pkg(test_path("testRbuildignore"))
  on.exit(unlink(test_pkg, recursive = TRUE))

  expect_equal(basename(package_files(test_pkg)), c("a.R", "ignore_me.R"))

  #writeLines("^R/ignore_me.R$", file.path(test_pkg, ".Rbuildignore"))
  writeChar("^R/ignore_me.R$\n", file.path(test_pkg, ".Rbuildignore"), eos = NULL)
  expect_equal(basename(package_files(test_pkg)), "a.R")
})
