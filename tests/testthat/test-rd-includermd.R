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

test_that("markdown with headers", {
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat(sep = "\n", file = tmp,
    "Text at the front",
    "",
    "# Header 1",
    "",
    "## Header 2",
    "",
    "Text",
    "",
    "## Header 22",
    "",
    "# Header 11",
    "",
    "Text again")
  rox <- sprintf("
    #' Title
    #' @includeRmd %s
    #' @name foobar
    NULL", tmp)
  out1 <- roc_proc_text(rd_roclet(), rox)[[1]]
  exp_details <- "Text at the front"
  exp_secs <- c(paste0(
    "\\section{Header 1}{",
    "\\subsection{Header 2}{Text}",
    "\\subsection{Header 22}{}",
    "}"),
    "\\section{Header 11}{Text again}"
  )
  expect_equal_strings(out1$fields$details$values, exp_details)
  expect_equal_strings(out1$fields$rawRd$values, exp_secs)
})

test_that("subsection within details", {
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat(sep = "\n", file = tmp,
    "Text at the front",
    "",
    "",
    "## Subsection in details",
    "",
    "Some subsection text")
  rox <- sprintf("
    #' Title
    #' @includeRmd %s
    #' @name foobar
    NULL", tmp)
  out1 <- roc_proc_text(rd_roclet(), rox)[[1]]
  exp_details <- paste0(
    "Text at the front",
    "\\subsection{Subsection in details}{ Some subsection text }"
  )
  expect_equal_strings(out1$fields$details$values, exp_details)
})

test_that("links to functions", {
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat(sep = "\n", file = tmp,
    "This is a link: [roxygenize()].",
    "Another one: [stringr::str_length()]"
  )
  rox <- sprintf("
    #' Title
    #' @includeRmd %s
    #' @name foobar
    NULL", tmp)
  out1 <- roc_proc_text(rd_roclet(), rox)[[1]]
  exp_details <- paste0(
    "This is a link: \\code{\\link[=roxygenize]{roxygenize()}}.",
    "Another one:\n\\code{\\link[stringr:str_length]{stringr::str_length()}}"
  )
  expect_equal_strings(out1$fields$details$values, exp_details)
})

test_that("links to functions, with anchors", {
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat(sep = "\n", file = tmp,
    "This is a link: [roxygenize()].",
    "Another one: [stringr::str_length()]",
    "",
    "[roxygenize()]: https://roxygen2.r-lib.org/reference/roxygenize.html",
    "[stringr::str_length()]: https://stringr.tidyverse.org/reference/str_length.html"
  )
  rox <- sprintf("
    #' Title
    #' @includeRmd %s
    #' @name foobar
    NULL", tmp)
  out1 <- roc_proc_text(rd_roclet(), rox)[[1]]
  exp_details <- paste0(
    "This is a link: \\code{\\link[=roxygenize]{roxygenize()}}.",
    "Another one:\n\\code{\\link[stringr:str_length]{stringr::str_length()}}"
  )
  expect_equal_strings(out1$fields$details$values, exp_details)
})
