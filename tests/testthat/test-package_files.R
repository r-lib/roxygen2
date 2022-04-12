test_that("respects order in Collate (#790)", {
  expect_equal(
    package_files('testCollateParse'),
    normalizePath(c("testCollateParse/R/b.r", "testCollateParse/R/c.r"))
  )
})

test_that("roxygen ignores files with matching pattern in .Rbuildignore", {
  local_package_copy(test_path("testRbuildignore"))

  expect_equal(basename(package_files()), c("a.R", "ignore_me.R"))

  write_lines("^R/ignore_me.R$", ".Rbuildignore")
  expect_equal(basename(package_files()), "a.R")

  # Continues to work even if empty lines or missing files
  write_lines(c("^R/ignore_me.R$", "", ".nonexistentfile"), ".Rbuildignore")
  expect_equal(basename(package_files()), "a.R")
})
