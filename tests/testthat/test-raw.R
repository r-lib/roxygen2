context("Raw output")

test_that("keywords split into pieces", {
  out <- roc_proc_text(rd_roclet(), "
    #' @rawRd #this is a comment
    #' @name a
    NULL")[[1]]

  lines <- strsplit(format(out), "\n")[[1]]
  expect_equal(lines[[5]], "#this is a comment")
})
