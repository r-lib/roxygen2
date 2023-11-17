test_that("useful error if no DESCRIPTION", {
  path <- local_package_copy(test_path("no-desc"), set_version = FALSE)

  expect_snapshot(
    roxygen_setup(path),
    error = TRUE,
    transform = function(x) gsub(path, "<path>", x)
  )
})

test_that("informs about initial setup", {
  path <- local_package_copy(test_path("empty"), set_version = FALSE)

  expect_snapshot(roxygen_setup(path, cur_version = "8.0.0"))
})

test_that("warns about non UTF-8 encoding", {
  path <- local_package_copy(test_path("empty"))
  desc::desc_set(file = path, Encoding = "latin1", RoxygenNote = "8.0.0")

  expect_snapshot(roxygen_setup(path, cur_version = "8.0.0"))
})

test_that("warns if roxygen version is too new", {
  path <- local_package_copy(test_path("empty"))
  desc::desc_set(file = path, RoxygenNote = "10.0.0")

  expect_snapshot(roxygen_setup(path, cur_version = "8.0.0"))
})

test_that("informs about major changes in 7.0.0", {
  path <- local_package_copy(test_path("empty"))
  desc::desc_set(file = path, RoxygenNote = "5.0.0")

  expect_snapshot(roxygen_setup(path, cur_version = "8.0.0"))
})
