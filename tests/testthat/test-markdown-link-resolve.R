test_that("don't resolve if current_package not set", {
  expect_equal(find_package("cli_abort"), NA_character_)
})

test_that("topics in current package don't need qualification", {
  local_roxy_meta_set("current_package", "cli")
  expect_equal(find_package("cli_abort"), NA_character_)
})

test_that("imported functions qualified with package name", {
  local_roxy_meta_set("current_package", "testMdLinks")
  local_roxy_meta_set("current_package_dir", test_path("testMdLinks"))

  expect_equal(find_package("cli_abort"), "cli")
})

test_that("base functions don't need qualification", {
  local_roxy_meta_set("current_package", "testMdLinks")
  local_roxy_meta_set("current_package_dir", test_path("testMdLinks"))

  expect_equal(find_package("mean"), NA_character_)
})

test_that("base functions re-exported by deps don't need qualification", {
  local_roxy_meta_set("current_package", "testMdLinks")
  local_roxy_meta_set("current_package_dir", test_path("testMdLinks"))

  expect_equal(find_package("is.null"), NA_character_)
})

test_that("useful warning if no topic found", {
  local_roxy_meta_set("current_package", "testMdLinks")
  local_roxy_meta_set("current_package_dir", test_path("testMdLinks"))

  expect_snapshot(find_package("doesntexist"))
})

test_that("re-exported topics are identified", {
  local_roxy_meta_set("current_package", "testMdLinks")
  local_roxy_meta_set("current_package_dir", test_path("testMdLinks"))

  expect_equal(find_package("process"), "processx")
})

test_that("gives useful warning if same name in multiple packages", {
  skip_on_cran() # in case pkgload/rlang changes this
  local_roxy_meta_set("current_package", "testMdLinks")
  local_roxy_meta_set("current_package_dir", test_path("testMdLinks"))

  expect_equal(
    find_package_dep_lookup("pkg_env", test_path("testMdLinks")),
    c("pkgload", "rlang")
  )

  expect_snapshot(find_package("pkg_env"))
})

test_that("find_source returns base package as-is", {
  skip_on_cran() # since depends on other packages

  expect_equal(find_source("list", "base"), "base")
  # topic not in namespace
  expect_equal(find_source("doesnt'exist", "cli"), "cli")
  # primitive objects are always in base
  expect_equal(find_source("is_null", "rlang"), "base")
  # callr re-exports process from processx
  expect_equal(find_source("process", "callr"), "processx")
})

test_that("find_source traces re-exported non-function to source package", {
  # .data is a non-function object exported by rlang, re-exported by others
  skip_on_cran()
  skip_if_not_installed("tidyselect")
  expect_equal(find_source(".data", "tidyselect"), "rlang")
})
