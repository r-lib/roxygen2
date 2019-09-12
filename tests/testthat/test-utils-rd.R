test_that("href doesn't get extra parens", {
  expect_equal(rd2text(parse_rd("\\href{a}{b}")), "\\href{a}{b}\n")
})
