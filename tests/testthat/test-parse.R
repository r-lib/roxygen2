test_that("can control processing order with @order", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @rdname bar
    #' @details 2
    #' @order 2
    foo <- function() {}

    #' @rdname bar
    #' @details 1
    #' @order 1
    bar <- function() {}

    #' @title Title
    #' @name bar
    NULL
  "
  )[[1]]

  expect_equal(out$get_value("details"), c("1", "2"))
})
