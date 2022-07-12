test_that("all exported tags included in tags.yml", {
  meta <- map_chr(tags_metadata(), "name")
  expect_setequal(meta, tags_list(FALSE))
})
