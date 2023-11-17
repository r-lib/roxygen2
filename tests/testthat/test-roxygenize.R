test_that("can regenerate NAMESPACE even if its broken", {
  local_package_copy(test_path("broken-namespace"))
  expect_snapshot(roxygenise("."))
})
