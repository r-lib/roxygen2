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

test_that("description block preserves whitespace", {
  out <- parse_text("
    #' Title
    #'
    #' Line 1
    #'   Line 2
    #'
    #' Line 1
    #'   Line 2
    f <- function() {}
    "
  )[[1]]

  expect_equal(out$description, "Line 1\n  Line 2")
  expect_equal(out$details, "Line 1\n  Line 2")
})
