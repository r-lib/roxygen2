test_that("generic keys produce expected output", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @title a
    #' @note test
    #' @author test
    #' @seealso test
    #' @references test
    #' @name a
    NULL"
  )[[1]]
  expect_equal(out$get_value("references"), "test")
  expect_equal(out$get_value("note"), "test")
  expect_equal(out$get_value("seealso"), "test")
  expect_equal(out$get_value("author"), "test")
})


test_that("author duplicated get removed", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @name a
    #' @title a
    #' @author A
    NULL

    #' @name b
    #' @rdname a
    #' @author A
    NULL

    #' @name c
    #' @rdname a
    #' @author B
    NULL"
  )[[1]]
  expect_equal(out$get_value("author"), c("A", "B"))
})

test_that("@format overrides defaults", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' @format abc
    #'
    x <- list(a = 1, b = 2)"
  )[[1]]

  expect_equal(out$get_value("format"), "abc")
})

test_that("@format NULL suppresses default usage", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' @format NULL
    x <- list(a = 1, b = 2)"
  )[[1]]

  expect_equal(out$get_value("format"), NULL)
})

test_that("@format not escaped", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #' @format %
    x <- list(a = 1, b = 2)"
  )[[1]]

  expect_equal(out$get_value("format"), "%")
  expect_equal(out$get_rd("format"), "\\format{\n%\n}")
})


test_that("@title warns about multiple paragraphs", {
  block <- "
    #' @title Paragraph 1
    #'
    #' Paragraph 2
    x <- list(a = 1, b = 2)
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))
})
