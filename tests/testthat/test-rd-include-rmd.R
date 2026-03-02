skip_if_not_installed("rmarkdown")

# clear some state
roxy_meta_clear()

test_that("invalid syntax gives useful warning", {
  block <- "
    #' @includeRmd
    NULL
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))
})

test_that("markdown file can be included", {
  skip_if_not(rmarkdown::pandoc_available("2.17"))

  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat(
    "List:\n\n* item1\n* item2\n\nInline `code` and _emphasis_.\n",
    file = tmp
  )
  rox <- sprintf(
    "
    #' Title
    #' @includeRmd %s
    #' @name foobar
    NULL",
    tmp
  )
  out1 <- roc_proc_text(rd_roclet(), rox)[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' @title Title
    #' @details List:
    #' \\itemize{
    #' \\item item1
    #' \\item item2
    #' }
    #'
    #' Inline \\code{code} and \\emph{emphasis}.
    #' @name foobar
    NULL"
  )[[1]]
  # make sure sections are in the same order
  expect_equal(sort(names(out1$sections)), sort(names(out2$sections)))
  out2$sections <- out2$sections[names(out1$sections)]
  expect_equivalent_rd(out1, out2)
})

test_that("markdown with headers", {
  skip_if_not(rmarkdown::pandoc_available("2.17"))

  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat(
    sep = "\n",
    file = tmp,
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
    "Text again"
  )
  rox <- sprintf(
    "
    #' Title
    #' @includeRmd %s
    #' @name foobar
    NULL",
    tmp
  )
  out1 <- roc_proc_text(rd_roclet(), rox)[[1]]
  exp_details <- "Text at the front"
  exp_secs <- list(
    title = c("Header 1", "Header 11"),
    content = c(
      "\\subsection{Header 2}{\n\nText\n}\n\n\\subsection{Header 22}{\n}",
      "Text again"
    )
  )
  expect_equal_strings(out1$get_value("details"), exp_details)
  expect_equal_strings(out1$get_value("section"), exp_secs)
})

test_that("subsection within details", {
  skip_if_not(rmarkdown::pandoc_available("2.17"))

  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat(
    sep = "\n",
    file = tmp,
    "Text at the front",
    "",
    "",
    "## Subsection in details",
    "",
    "Some subsection text"
  )
  rox <- sprintf(
    "
    #' Title
    #' @includeRmd %s
    #' @name foobar
    NULL",
    tmp
  )
  out1 <- roc_proc_text(rd_roclet(), rox)[[1]]
  exp_details <- paste0(
    "Text at the front",
    "\\subsection{Subsection in details}{ Some subsection text }"
  )
  expect_equal_strings(out1$get_value("details"), exp_details)
})

test_that("links to functions", {
  skip_if_not(rmarkdown::pandoc_available("2.17"))

  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat(
    sep = "\n",
    file = tmp,
    "This is a link: [roxygenize()].",
    "Another one: [stringr::str_length()]"
  )
  rox <- sprintf(
    "
    #' Title
    #' @includeRmd %s
    #' @name foobar
    NULL",
    tmp
  )
  out1 <- roc_proc_text(rd_roclet(), rox)[[1]]
  exp_details <- paste0(
    "This is a link: \\code{\\link[=roxygenize]{roxygenize()}}.",
    "Another one:\n\\code{\\link[stringr:str_length]{stringr::str_length()}}"
  )
  expect_equal_strings(out1$get_value("details"), exp_details)
})

test_that("links to functions, with anchors", {
  skip_if_not(rmarkdown::pandoc_available("2.17"))

  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat(
    sep = "\n",
    file = tmp,
    "This is a link: [roxygenize()].",
    "Another one: [stringr::str_length()]",
    "",
    "[roxygenize()]: https://roxygen2.r-lib.org/reference/roxygenize.html",
    "[stringr::str_length()]: https://stringr.tidyverse.org/reference/str_length.html"
  )
  rox <- sprintf(
    "
    #' Title
    #' @includeRmd %s
    #' @name foobar
    NULL",
    tmp
  )
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
  tag <- roxy_test_tag()

  cat("", sep = "", file = tmp)
  expect_equal(rmd_eval_rd(tmp, tag), structure("", names = ""))

  cat("  ", sep = "", file = tmp)
  expect_equal(rmd_eval_rd(tmp, tag), structure("", names = ""))

  cat("\n", sep = "", file = tmp)
  expect_equal(rmd_eval_rd(tmp, tag), structure("", names = ""))
})

test_that("inline html", {
  skip_if_not(rmarkdown::pandoc_available("2.17"))

  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat(
    sep = "\n",
    file = tmp,
    "Text at the front",
    "",
    "",
    "## Subsection in details",
    "",
    "Some subsection text with <span class='x'>inline html</span>."
  )
  rox <- sprintf(
    "
    #' Title
    #' @includeRmd %s
    #' @name foobar
    NULL",
    tmp
  )
  out1 <- roc_proc_text(rd_roclet(), rox)[[1]]
  exp_details <- paste0(
    "Text at the front",
    "\\subsection{Subsection in details}{",
    "Some subsection text with ",
    "\\if{html}{\\out{<span class=\"x\">}}inline html\\if{html}{\\out{</span>}}.\n}"
  )
  expect_equal_strings(out1$get_value("details"), exp_details)
})

test_that("html block", {
  skip_if_not(rmarkdown::pandoc_available("2.17"))

  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat(
    sep = "\n",
    file = tmp,
    "Text at the front",
    "",
    "<a id=\"test\"></a>",
    "",
    "Text"
  )
  rox <- sprintf(
    "
    #' Title
    #' @includeRmd %s
    #' @name foobar
    NULL",
    tmp
  )
  out1 <- roc_proc_text(rd_roclet(), rox)[[1]]
  exp_details <- paste0(
    "Text at the front",
    "\\if{html}{\\out{<a id=\"test\">}}\\if{html}{\\out{</a>}}",
    "Text"
  )
  expect_equal_strings(out1$get_value("details"), exp_details)
})

test_that("include as another section", {
  skip_if_not(rmarkdown::pandoc_available("2.17"))

  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat(
    "List:\n\n* item1\n* item2\n\nInline `code` and _emphasis_.\n",
    file = tmp
  )
  rox <- sprintf(
    "
    #' Title
    #' @includeRmd %s description
    #' @name foobar
    NULL",
    tmp
  )
  out1 <- roc_proc_text(rd_roclet(), rox)[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' @title Title
    #' @description List:
    #' \\itemize{
    #' \\item item1
    #' \\item item2
    #' }
    #'
    #' Inline \\code{code} and \\emph{emphasis}.
    #' @name foobar
    NULL"
  )[[1]]
  # make sure sections are in the same order
  expect_equal(sort(names(out1$sections)), sort(names(out2$sections)))
  out2$sections <- out2$sections[names(out1$sections)]
  expect_equivalent_rd(out1, out2)
})

test_that("order of sections is correct", {
  skip_if_not(rmarkdown::pandoc_available("2.17"))

  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat(
    "# Rmd\n\nList:\n\n* item1\n* item2\n\nInline `code` and _emphasis_.\n",
    file = tmp
  )
  rox <- sprintf(
    "
    #' Title
    #' @description desc
    #' @details details
    #' @includeRmd %s
    #' @section After:
    #' This is after.
    #' @section After2:
    #' This is even more after.
    #' @name foobar
    NULL",
    tmp
  )
  out1 <- roc_proc_text(rd_roclet(), rox)[[1]]
  expect_match(format(out1), "Rmd.*After.*After2")
})

test_that("useful warnings", {
  skip_if_not(rmarkdown::pandoc_available("2.17"))

  block <- "
    #' Title
    #' @includeRmd path
    #' @name foobar
    NULL"
  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))

  path <- withr::local_tempfile(
    fileext = ".Rmd",
    lines = c(
      "```{r}",
      "stop('Error')",
      "```"
    )
  )
  path <- normalizePath(path)

  text <- sprintf(
    "
    #' Title
    #' @includeRmd %s
    #' @name foobar
    NULL",
    path
  )
  expect_snapshot(
    . <- roc_proc_text(rd_roclet(), text),
    transform = function(x) {
      x <- gsub(path, "<temp-path.Rmd>", x, fixed = TRUE)
      x <- gsub("file.*\\.Rmd", "<another-temp-path.Rmd>", x)
      line <- grep("~~~", x)[1]
      if (!is.na(line)) {
        x <- x[1:(line - 1)]
      }
      x
    }
  )
})

test_that("sets width", {
  skip_if_not(rmarkdown::pandoc_available("2.17"))

  local_options(width = 123)
  temp_rd <- withr::local_tempfile(lines = "`r getOption('width')`")

  rox <- sprintf(
    "
    #' Title
    #' @includeRmd %s
    #' @name foobar
    NULL",
    temp_rd
  )
  out <- roc_proc_text(rd_roclet(), rox)[[1]]
  expect_equal(out$get_value("details"), "80")
})
