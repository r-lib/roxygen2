test_that("all exported tags included in tags.yml", {
  expect_setequal(tags_metadata()$tag, tags_list(FALSE))
})
