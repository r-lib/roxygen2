context("Safety")

test_that("roxygenize stops without side effects if no DESCRIPTION found", {
  path <- test_path("no-desc")
  namespace <- file.path(path, "NAMESPACE")

  expect_error(roxygenize(path), "must include a DESCRIPTION file")
  expect_equal(list.files(path), c("NAMESPACE", "R"))
  expect_false(made_by_roxygen(namespace))
})
