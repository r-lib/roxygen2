test_that("missing title generates useful message", {
  block <- "
    #' @name foo
    NULL
  "
  expect_snapshot(roc_proc_text(rd_roclet(), block))
})
