context("RdTopic")

test_that("adding tags merges with existing", {
  rd <- RdTopic$new()

  rd$add_tags(new_tag("x", 1))
  rd$add_tags(new_tag("x", 2))

  expect_equal(rd$get_tag("x")$values, c(1, 2))
})

test_that("unless overwrite = TRUE", {
  rd <- RdTopic$new()

  rd$add_tags(new_tag("x", 1))
  rd$add_tags(new_tag("x", 2), overwrite = TRUE)

  expect_equal(rd$get_tag("x")$values, 2)
})

test_that("can add a complete file", {
  rd1 <- RdTopic$new()
  rd2 <- RdTopic$new()

  rd1$add_tags(new_tag("x", 1))
  rd2$add_tags(new_tag("x", 2))
  rd2$add_file(rd1)

  expect_equal(rd2$get_tag("x")$values, c(2, 1))
})
