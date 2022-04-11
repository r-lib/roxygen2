test_that("useful error if no DESCRIPTION", {
  local_package_copy(test_path("no-desc"), set_version = FALSE)

  expect_snapshot(roxygen_setup(), error = TRUE)
})

test_that("informs about initial setup", {
  local_package_copy(test_path("empty"), set_version = FALSE)

  expect_snapshot(roxygen_setup(cur_version = "8.0.0"))
})

test_that("warns about non UTF-8 encoding", {
  local_package_copy(test_path("empty"))
  desc::desc_set(Encoding = "latin1", RoxygenNote = "8.0.0")

  expect_snapshot(roxygen_setup(cur_version = "8.0.0"))
})

test_that("warns if roxygen version is too new", {
  local_package_copy(test_path("empty"))
  desc::desc_set(RoxygenNote = "10.0.0")

  expect_snapshot(roxygen_setup(cur_version = "8.0.0"))
})

test_that("informs about major changes in 7.0.0", {
  local_package_copy(test_path("empty"))
  desc::desc_set(RoxygenNote = "5.0.0")

  expect_snapshot(roxygen_setup(cur_version = "8.0.0"))
})
