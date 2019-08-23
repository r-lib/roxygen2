test_that("respects order in Collate (#790)", {
  expect_equal(
    package_files('testCollateParse'),
    normalizePath(c("testCollateParse/R/b.r", "testCollateParse/R/c.r"))
  )
})

test_that("roxygen ignores files with matching pattern in .Rbuildignore", {
  test_pkg <- temp_copy_pkg(test_path("testRbuildignore"))
  on.exit(unlink(test_pkg, recursive = TRUE))

  expect_equal(basename(package_files(test_pkg)), c("a.R", "ignore_me.R"))

  write_lines("^R/ignore_me.R$\n", file.path(test_pkg, ".Rbuildignore"))
  expect_equal(basename(package_files(test_pkg)), "a.R")
})

test_that("roxygen works with empty lines in .Rbuildignore", {
  test_pkg <- temp_copy_pkg(test_path("testRbuildignore"))
  on.exit(unlink(test_pkg, recursive = TRUE))

  write_lines("^R/ignore_me.R$\n\n.nonexistentfile", file.path(test_pkg, ".Rbuildignore"))
  expect_equal(basename(package_files(test_pkg)), "a.R")
})
