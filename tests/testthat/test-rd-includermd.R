test_that("markdown file can be included", {
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat("List:\n\n* item1\n* item2\n\nInline `code` and _emphasis_.\n",
      file = tmp)
  rox <- sprintf("
    #' Title
    #' @includeRmd %s
    #' @name foobar
    NULL", tmp)
  out1 <- roc_proc_text(rd_roclet(), rox)[[1]]
  out1$fields$details$values <- str_trim(out1$fields$details$values)
  out2 <- roc_proc_text(rd_roclet(), "
    #' @details
    #'
    #'
    #' List:
    #' \\itemize{
    #' \\item item1
    #' \\item item2
    #' }
    #'
    #' Inline \\code{code} and \\emph{emphasis}.
    #' @title Title
    #' @name foobar
    NULL")[[1]]
  expect_equivalent_rd(out1, out2)
})
