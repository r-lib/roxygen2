test_that("empty description block is silently removed", {
  expect_warning(
    roc_proc_text(rd_roclet(), "
      #'
      #'
      f <- function() {}
      "
    ),
    NA
  )
})
