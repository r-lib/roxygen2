context("Topic name")

test_that("refclass topicname has ref-class prefix", {
  setRefClass("X1")
  on.exit(removeClass("X1"))
  obj <- object("rcclass", "X1", getRefClass("X1"))
  expect_equal(topic_name(obj), "X1-ref-class")
})

test_that("class topicname has class prefix", {
  setClass("Y1")
  on.exit(removeClass("Y1"))
  obj <- object("s4class", "Y1", getClass("Y1"))
  expect_equal(topic_name(obj), "Y1-class")
})