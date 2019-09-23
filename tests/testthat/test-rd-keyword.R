context("Rd: keyword")

test_that("keywords split into pieces", {
  out <- roc_proc_text(rd_roclet(), "
    #' @keywords a b
    #' @title a
    #' @name a
    NULL")[[1]]

  expect_equal(out$get_value("keyword"), c("a", "b"))
})
