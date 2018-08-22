context("Source")

test_that("Leading whitespace in Collate field creates no problems", {
  test_pkg <- temp_copy_pkg('testCollateNoIncludes')
  on.exit(unlink(test_pkg, recursive = TRUE))
  expect_length(package_files(test_pkg), 2)
})
