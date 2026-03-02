test_that("one line per concept", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @title a
    #' @name a
    #' @concept test1
    #' @concept test2
    NULL"
  )[[1]]

  expect_equal(out$get_value("concept"), c("test1", "test2"))
  expect_equal(out$get_rd("concept"), c("\\concept{test1}", "\\concept{test2}"))
})

test_that("simple keys produce expected output", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @title a
    #' @encoding test
    #' @name a
    NULL"
  )[[1]]
  expect_equal(out$get_value("encoding"), "test")
})

test_that("keywords split into pieces", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @keywords a b
    #' @title a
    #' @name a
    NULL"
  )[[1]]

  expect_equal(out$get_value("keyword"), c("a", "b"))
})
