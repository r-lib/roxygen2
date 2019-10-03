test_that("markdown file can be included", {
  skip_if_not(rmarkdown::pandoc_available())

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
  out2 <- roc_proc_text(rd_roclet(), "
    #' @title Title
    #' @details List:
    #' \\itemize{
    #' \\item item1
    #' \\item item2
    #' }
    #'
    #' Inline \\code{code} and \\emph{emphasis}.
    #' @name foobar
    NULL")[[1]]
  # make sure sections are in the same order
  expect_equal(sort(names(out1$sections)), sort(names(out2$sections)))
  out2$sections <- out2$sections[names(out1$sections)]
  expect_equivalent_rd(out1, out2)
})

test_that("markdown with headers", {
  skip_if_not(rmarkdown::pandoc_available())

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
  expect_equal_strings(out1$get_value("details"), exp_details)
  expect_equal_strings(out1$get_value("rawRd"), exp_secs)
})

test_that("subsection within details", {
  skip_if_not(rmarkdown::pandoc_available())

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
  expect_equal_strings(out1$get_value("details"), exp_details)
})

test_that("links to functions", {
  skip_if_not(rmarkdown::pandoc_available())

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
  expect_equal_strings(out1$get_value("details"), exp_details)
})

test_that("links to functions, with anchors", {
  skip_if_not(rmarkdown::pandoc_available())

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
  expect_equal_strings(out1$get_value("details"), exp_details)
})

test_that("empty Rmd", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  tag <- roxy_tag("includeRmd", tmp)

  cat("", sep = "", file = tmp)
  expect_equal(rmd_eval_rd(tmp, tag), "")

  cat("  ", sep = "", file = tmp)
  expect_equal(rmd_eval_rd(tmp, tag), "")

  cat("\n", sep = "", file = tmp)
  expect_equal(rmd_eval_rd(tmp, tag), "")
})

test_that("inline html", {
  skip_if_not(rmarkdown::pandoc_available())

  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat(sep = "\n", file = tmp,
    "Text at the front",
    "",
    "",
    "## Subsection in details",
    "",
    "Some subsection text with <span>inline html</span>.")
  rox <- sprintf("
    #' Title
    #' @includeRmd %s
    #' @name foobar
    NULL", tmp)
  out1 <- roc_proc_text(rd_roclet(), rox)[[1]]
  exp_details <- paste0(
    "Text at the front",
    "\\subsection{Subsection in details}{",
    "Some subsection text with ",
    "\\if{html}{\\out{<span>}}inline html\\if{html}{\\out{</span>}}.\n}"
  )
  expect_equal_strings(out1$get_value("details"), exp_details)
})

test_that("html block", {
  skip_if_not(rmarkdown::pandoc_available())

  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat(sep = "\n", file = tmp,
    "Text at the front",
    "",
    "<a id=\"test\"></a>",
    "",
    "Text")
  rox <- sprintf("
    #' Title
    #' @includeRmd %s
    #' @name foobar
    NULL", tmp)
  out1 <- roc_proc_text(rd_roclet(), rox)[[1]]
  exp_details <- paste0(
    "Text at the front",
    "\\if{html}{\\out{<a id=\"test\">}}\\if{html}{\\out{</a>}}",
    "Text"
  )
  expect_equal_strings(out1$get_value("details"), exp_details)
})
