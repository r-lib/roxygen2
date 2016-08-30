context("RoxyTopic")

test_that("adding tags merges with existing", {
  rd <- RoxyTopic$new()

  rd$add(new_tag("x", 1))
  rd$add(new_tag("x", 2))

  expect_equal(rd$get_tag("x")$values, c(1, 2))
})

test_that("unless overwrite = TRUE", {
  rd <- RoxyTopic$new()

  rd$add(new_tag("x", 1))
  rd$add(new_tag("x", 2), overwrite = TRUE)

  expect_equal(rd$get_tag("x")$values, 2)
})

test_that("can add a complete file", {
  rd1 <- RoxyTopic$new()
  rd2 <- RoxyTopic$new()

  rd1$add(new_tag("x", 1))
  rd2$add(new_tag("x", 2))
  rd2$add(rd1)

  expect_equal(rd2$get_tag("x")$values, c(2, 1))
})
