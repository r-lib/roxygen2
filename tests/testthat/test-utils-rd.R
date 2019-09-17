test_that("href doesn't get extra parens", {
  expect_equal(rd2text(parse_rd("\\href{a}{b}")), "\\href{a}{b}\n")
})

test_that("can tweak links to point to new package", {
  rd1 <- tweak_links(parse_rd("\\link{foo}"), package = "bar")
  rd2 <- tweak_links(parse_rd("\\link[baz]{foo}"), package = "bar")

  expect_equal(rd2text(rd1), "\\link[bar]{foo}\n")
  expect_equal(rd2text(rd2), "\\link[baz]{foo}\n")
})
