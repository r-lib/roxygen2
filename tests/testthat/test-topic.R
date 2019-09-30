test_that("adding tags merges with existing", {
  rd <- RoxyTopic$new()

  rd$add(rd_section("x", 1))
  rd$add(rd_section("x", 2))

  expect_equal(rd$get_value("x"), c(1, 2))
})

test_that("unless overwrite = TRUE", {
  rd <- RoxyTopic$new()

  rd$add(rd_section("x", 1))
  rd$add(rd_section("x", 2), overwrite = TRUE)

  expect_equal(rd$get_value("x"), 2)
})

test_that("can add a complete file", {
  rd1 <- RoxyTopic$new()
  rd2 <- RoxyTopic$new()

  rd1$add(rd_section("x", 1))
  rd2$add(rd_section("x", 2))
  rd2$add(rd1)

  expect_equal(rd2$get_value("x"), c(2, 1))
})
