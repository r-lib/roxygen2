context("File names")

test_that("nice_name leaves ok chars unchanged", {
  expect_equal(nice_name("abc"), "abc")
  expect_equal(nice_name("a_b-c.R"), "a_b-c.R")
})

test_that("nice_name protects against invalid characters", {
  expect_equal(nice_name("a<-"), "a-set")
  expect_equal(nice_name("[.a"), "sub-.a")
})

