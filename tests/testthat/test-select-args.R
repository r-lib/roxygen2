test_that("warns on invalid input", {
  expect_snapshot({
    select_args_text(c("x", "na.rm"), "-xlab:", "test")
    select_args_text(c("x", "na.rm"), '"a"', "test")
    select_args_text(c("x", "y", "z"), "-x:z", "test")
  })
})

test_that("positive initial values starts from nothing", {
  expect_equal(select_args_text(c("x", "y", "z"), "x y", "test"), c("x", "y"))
})

test_that("negative initial starts from everything", {
  expect_equal(select_args_text(c("x", "y", "z"), "-z", "test"), c("x", "y"))
})

test_that("can alternative exclusion and inclusion", {
  expect_equal(select_args_text(c("x", "y", "z"), "-z z", "test"), c("x", "y", "z"))
  expect_equal(select_args_text(c("x", "y", "z"), "z -z", "test"), character())
})
