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
  desc::desc_set(
    file = path,
    Encoding = "latin1",
    "Config/roxygen2/version" = "8.0.0"
  )

  expect_snapshot(roxygen_setup(path, cur_version = "8.0.0"))
})

test_that("warns if roxygen version is too new", {
  path <- local_package_copy(test_path("empty"))
  desc::desc_set(file = path, "Config/roxygen2/version" = "10.0.0")

  expect_snapshot(roxygen_setup(path, cur_version = "8.0.0"))
})

test_that("informs about major changes in 7.0.0", {
  path <- local_package_copy(test_path("empty"), set_version = FALSE)
  desc::desc_set(file = path, RoxygenNote = "5.0.0")

  expect_snapshot(roxygen_setup(path, cur_version = "8.0.0"))
})

test_that("removes old RoxygenNote field", {
  path <- local_package_copy(test_path("empty"), set_version = FALSE)
  desc::desc_set(file = path, RoxygenNote = "7.0.0")

  suppressMessages(roxygen_setup(path, cur_version = "8.0.0"))

  desc <- desc::desc(file = path)
  expect_false(desc$has_fields("RoxygenNote"))
  expect_equal(desc$get("Config/roxygen2/version")[[1]], "8.0.0")
})

test_that("removes old RoxygenNote field even if already migrated (#1876)", {
  path <- local_package_copy(test_path("empty"))
  desc::desc_set(
    file = path,
    "Config/roxygen2/version" = "8.0.0",
    RoxygenNote = "7.3.3"
  )

  expect_snapshot(roxygen_setup(path, cur_version = "8.0.0"))

  desc <- desc::desc(file = path)
  expect_false(desc$has_fields("RoxygenNote"))
  expect_equal(desc$get("Config/roxygen2/version")[[1]], "8.0.0")
})

test_that("doesn't touch DESCRIPTION if already up to date", {
  path <- local_package_copy(test_path("empty"))
  desc::desc_set(file = path, "Config/roxygen2/version" = "8.0.0")

  desc_path <- file.path(path, "DESCRIPTION")
  before <- file.mtime(desc_path)
  expect_snapshot(roxygen_setup(path, cur_version = "8.0.0"))
  expect_equal(file.mtime(desc_path), before)
})
