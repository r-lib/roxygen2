test_that("resolve_link_package", {
  # Doesn't work with
  skip_if_not(
    is.null(.getNamespace("roxygen2")[[".__DEVTOOLS__"]]),
    "roxygen2 loaded with devtools"
  )
  expect_snapshot({
    resolve_link_package("roxygenize", "roxygen2", test_path("testMdLinks2"))
    resolve_link_package("UseMethod", "roxygen2", test_path("testMdLinks2"))
    resolve_link_package("cli_abort", "roxygen2", test_path("testMdLinks2"))
  })

  tag <- roxy_tag("title", NULL, NULL, file = "foo.R", line = 10)
  expect_snapshot({
    resolve_link_package(
      "aa3bc042880aa3b64fef1a9",
      "roxygen2",
      test_path("testMdLinks2"),
      list(tag = tag)
    )
  })
  # re-exported topics are identified
  expect_equal(
    resolve_link_package("process", "testthat", test_path("testMdLinks")),
    "processx"
  )
})

test_that("resolve_link_package name clash", {
  # skip in case pkgload/rlang changes this
  skip_on_cran()
  tag <- roxy_tag("title", NULL, NULL, file = "foo.R", line = 10)
  expect_snapshot({
    resolve_link_package(
      "pkg_env",
      "roxygen2",
      test_path("testMdLinks2"),
      list(tag = tag)
    )
  })
})

test_that("find_reexport_source", {
  expect_equal(find_reexport_source("process", "callr"), "processx")
  expect_null(find_reexport_source("list", "base"))
})
