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

  expect_snapshot(. <- find_package("doesntexist"))
})

test_that("re-exported topics are identified", {
  local_roxy_meta_set("current_package", "testMdLinks")
  local_roxy_meta_set("current_package_dir", test_path("testMdLinks"))

  expect_equal(. <- find_package("process"), "processx")
})

test_that("gives useful warning if same name in multiple packages", {
  skip_on_cran() # in case pkgload/rlang changes this
  local_roxy_meta_set("current_package", "testMdLinks")
  local_roxy_meta_set("current_package_dir", test_path("testMdLinks"))

  expect_snapshot(. <- find_package("pkg_env"))
})


test_that("topic found in multiple base packages doesn't warn", {
  local_roxy_meta_set("current_package", "testMdLinks")
  local_roxy_meta_set("current_package_dir", test_path("testMdLinks"))

  # plot is in both base and graphics
  expect_no_message(expect_equal(find_package("plot"), NA_character_))
})
