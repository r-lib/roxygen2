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

  expect_equal(
    find_package_lookup("pkg_env", "testMdLinks", test_path("testMdLinks")),
    c("pkgload", "rlang")
  )

  expect_snapshot(. <- find_package("pkg_env"))
})


test_that("topic found in multiple base packages doesn't warn", {
  local_roxy_meta_set("current_package", "testMdLinks")
  local_roxy_meta_set("current_package_dir", test_path("testMdLinks"))

  # plot is in both base and graphics
  expect_no_message(expect_equal(find_package("plot"), NA_character_))
})

test_that("has_topic uses the installed rdtools index", {
  expect_true(has_topic("mean", "base"))
  expect_false(has_topic("no-such-topic", "base"))
  expect_false(has_topic("mean", "no-such-package"))
})

test_that("has_topic uses the rdtools index for source packages", {
  # roxygen2 itself is either loaded from source (devtools::test()) or
  # installed (R CMD check); both branches must find its topics
  expect_true(has_topic("roxygenize", "roxygen2"))
  expect_false(has_topic("no-such-topic", "roxygen2"))
})
