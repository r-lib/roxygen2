test_that("can regenerate NAMESPACE even if its broken", {
  path <- local_package_copy(test_path("broken-namespace"))
  expect_snapshot(roxygenise(path))
})
