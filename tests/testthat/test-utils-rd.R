test_that("has_topic() works as you'd expect", {
  expect_equal(has_topic("abbreviate", "base"), TRUE)
  expect_equal(has_topic("abbreviateXXXX", "base"), FALSE)

  expect_equal(has_topic("foo", "PACKAGEDOESNOTEXIST"), FALSE)
})
