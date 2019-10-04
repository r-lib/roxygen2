test_that("href doesn't get extra parens", {
  expect_equal(rd2text(parse_rd("\\href{a}{b}")), "\\href{a}{b}\n")
})

test_that("can tweak links to point to new package", {
  rd1 <- tweak_links(parse_rd("\\link{roclet}"), package = "roxygen2")
  rd2 <- tweak_links(parse_rd("\\link[roxygen2]{roclet}"), package = "roxygen2")

  expect_equal(rd2text(rd1), "\\link[roxygen2]{roclet}\n")
  expect_equal(rd2text(rd2), "\\link[roxygen2]{roclet}\n")
})
