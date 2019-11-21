test_that("href doesn't get extra parens", {
  expect_equal(rd2text(parse_rd("\\href{a}{b}")), "\\href{a}{b}\n")
})

test_that("has_topic() works as you'd expect", {
  expect_equal(has_topic("abbreviate", "base"), TRUE)
  expect_equal(has_topic("abbreviateXXXX", "base"), FALSE)

  expect_equal(has_topic("foo", "PACKAGEDOESNOTEXIST"), FALSE)
})

test_that("can tweak links to point to new package", {
  rd1 <- tweak_links(parse_rd("\\link{abbreviate}"), package = "base")
  rd2 <- tweak_links(parse_rd("\\link[base]{abbreviate}"), package = "base")
  rd3 <- tweak_links(parse_rd("\\link[=abbreviate]{abbr}"), package = "base")

  expect_equal(rd2text(rd1), "\\link[base]{abbreviate}\n")
  expect_equal(rd2text(rd2), "\\link[base]{abbreviate}\n")
  expect_equal(rd2text(rd3), "\\link[base:abbreviate]{abbr}\n")
})
