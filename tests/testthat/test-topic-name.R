context("Topic name")

test_that("name captured from assignment", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    a <- function() {} ")[[1]]

  expect_equal(get_tag(out, "name")$values, "a")
  expect_equal(get_tag(out, "alias")$values, "a")
  expect_equal(get_tag(out, "title")$values, "Title.")
})

test_that("name also captured from assignment by =", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    a = function() {} ")[[1]]

  expect_equal(get_tag(out, "name")$values, "a")
  expect_equal(get_tag(out, "alias")$values, "a")
  expect_equal(get_tag(out, "title")$values, "Title.")
})


test_that("`$` not to be parsed as assignee in foo$bar(a = 1)", {
  out <- roc_proc_text(rd_roclet(), "
    #' foo object
    foo <- list(bar = function(a) a)
    foo$bar(a = 1)")[[1]]

  expect_equal(get_tag(out, "name")$values, "foo")
})


test_that("names escaped, not quoted", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    '%a%' <- function(x, y) x + y")[[1]]
  expect_equal(format(get_tag(out, "name")), "\\name{\\%a\\%}\n")
})

test_that("quoted names captured from assignment", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    \"myfunction\" <- function(...) {}")[[1]]

  expect_equal(get_tag(out, "name")$values, "myfunction")
  expect_equal(get_tag(out, "alias")$values, "myfunction")

  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    `myfunction` <- function(...) {}")[[1]]
  expect_equal(get_tag(out, "name")$values, "myfunction")
  expect_equal(get_tag(out, "alias")$values, "myfunction")

  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    \"my function\" <- function(...) {}")[[1]]

  expect_equal(get_tag(out, "name")$values, "my function")
  expect_equal(get_tag(out, "alias")$values, "my function")
})

test_that("@name overides default", {
  out <- roc_proc_text(rd_roclet(), "
    #' @name b
    a <- function() {}")[[1]]

  expect_equal(get_tag(out, "name")$values, "b")
  expect_equal(sort(get_tag(out, "alias")$values), c("a", "b"))
})

test_that("refclass topicname has class prefix", {
  env <- pkg_env()
  setRefClass("X1", where = env)
  on.exit(removeClass("X1", where = env))
  obj <- object(getClass("X1", where = env))
  expect_equal(default_topic_name(obj), "X1-class")
})

test_that("class topicname has class prefix", {
  env <- pkg_env()
  setClass("Y1", where = env)
  on.exit(removeClass("Y1", where = env))
  obj <- object(getClass("Y1", where = env))
  expect_equal(default_topic_name(obj), "Y1-class")
})
