test_that("warn about unknown tags", {
  block <- "
    #' @unknown
    foo <- function() {}
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))
})

test_that("incomplete rd in prequel or tag raises issue", {
  block <- "
    #' Title {
    #' @aliases title{
    x <- 1
  "
  expect_snapshot(out <- roc_proc_text(rd_roclet(), block))
  expect_equal(out[[1]]$get_value("title"), "")
  expect_equal(out[[1]]$get_value("alias"), "x")
})
