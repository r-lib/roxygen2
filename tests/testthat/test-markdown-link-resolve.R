test_that("imported functions qualified with package name", {
  expect_equal(
    resolve_link_package("cli_abort", pkgdir = test_path("testMdLinks")),
    "cli"
  )
})

test_that("base functions don't need qualification", {
  # base packages don't need qualification
  expect_equal(
    resolve_link_package("mean", pkgdir = test_path("testMdLinks")),
    NA_character_
  )
})

test_that("topics in current package don't need qualification", {
  expect_equal(
    resolve_link_package("cli_abort", "cli", pkgdir = test_path("testMdLinks")),
    NA_character_
  )
})

test_that("gives useful warning if no topic found", {
  expect_snapshot({
    resolve_link_package("doesntexist", "roxygen2", test_path("testMdLinks"))
  })
})

test_that("re-exported topics are identified", {
  expect_equal(
    resolve_link_package("process", pkgdir = test_path("testMdLinks")),
    "processx"
  )
})

test_that("gives useful warning if same name in multiple packages", {
  # skip in case pkgload/rlang changes this
  skip_on_cran()
  expect_snapshot({
    resolve_link_package("pkg_env", "roxygen2", test_path("testMdLinks"))
  })
})

test_that("find_reexport_source", {
  expect_equal(find_reexport_source("process", "callr"), "processx")
  expect_null(find_reexport_source("list", "base"))
})
