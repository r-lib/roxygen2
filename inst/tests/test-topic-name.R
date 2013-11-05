context("Topic name")

test_that("refclass topicname has ref-class prefix", {
  X <- setRefClass("X")
  obj <- object("rcclass", "X", X)
  expect_equal(topic_name(obj), "X-ref-class")
})