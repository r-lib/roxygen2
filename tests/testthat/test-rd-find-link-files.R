test_that("generates informative warnings", {
  expect_snapshot({
    tag <- roxy_test_tag()
    find_topic_filename("11papaya", "foo", tag)
    find_topic_filename("stringr", "foofofofoo", tag)
  })
})
