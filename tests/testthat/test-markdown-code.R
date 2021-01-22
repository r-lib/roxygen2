test_that("can eval inline code", {
  example <- test_path("roxygen-block-md-code-simple-inline.R")
  out1 <- roc_proc_text(rd_roclet(), brio::read_file(example))[[1]]
  expect_equal(out1$get_value("title"), "Title 2")
  expect_equal(out1$get_value("description"), "Description 4")
})

test_that("can eval fenced code", {
  example <- test_path("roxygen-block-md-code-simple-fenced.R")
  out1 <- roc_proc_text(rd_roclet(), brio::read_file(example))[[1]]
  expect_match(out1$get_value("details"), "2")
})

test_that("use same env within, but not across blocks", {
  example <- test_path("roxygen-block-md-code-envs.R")
  out1 <- roc_proc_text(rd_roclet(), brio::read_file(example))[[1]]
  out2 <- roc_proc_text(rd_roclet(), brio::read_file(example))[[2]]
  expect_equal(out1$get_value("title"), "Title  420")
  expect_equal(out1$get_value("description"), "Description TRUE")
  expect_equal(out2$get_value("description"), "Description FALSE")
})

test_that("appropriate knit print method for fenced and inline is applied", {
  knit_print.foo <- function(x, inline = FALSE, ...) {
    knitr::asis_output(ifelse(inline, "inline", "fenced"))
  }
  library(knitr)
  on.exit(detach(package:knitr), add = TRUE, after = FALSE)
  registerS3method(
    genname = "knit_print", 
    class = "foo", 
    method = "knit_print.foo"
  )
  out1 <- roc_proc_text(rd_roclet(), "
    #' @title Title `r structure('default', class = 'foo')`
    #' 
    #' @details Details
    #'
    #' ```{r}
    #' structure('default', class = 'foo')
    #' ```
    #'
    #' @md
    #' @name bar
    NULL
  ")
  expect_match(out1$bar.Rd$get_value("details"), "fenced", fixed = TRUE)
  expect_match(out1$bar.Rd$get_value("title"), "inline", fixed = TRUE)
})

test_that("can create markdown markup", {
  expect_identical(
    markdown("Description `r paste0('_', 'keyword', '_')`"),
    "Description \\emph{keyword}"
  )
})

test_that("can create markdown markup piecewise", {
  expect_identical(
    markdown(
      "Description [`r paste0('https://url')`](`r paste0('link text')`)"
    ),
    "Description \\link{https://url}(link text)"
  )
})

test_that("can create escaped markdown markup", {
  # this workaround is recommended by @yihui
  # "proper" escaping for inline knitr tracked in https://github.com/yihui/knitr/issues/1704
  example <- test_path("roxygen-block-md-code-backtick.R")
  out1 <- roc_proc_text(rd_roclet(), brio::read_file(example))[[1]]
  expect_match(out1$get_value("title"), "\\code{bar}", fixed = TRUE)
})

test_that("NULL creates no text", {
  expect_identical(
    markdown("Description --`r NULL`--"),
    "Description ----"
  )
})

test_that("various errors", {
  verify_output(test_path("markdown-code-errors.txt"), {

    # multi-line inline code block
    roc_proc_text(rd_roclet(), "
      #' Title
      #'
      #' Description --`r 1 +
      #'   1`--
      #' @md
      #' @name dummy
      NULL"
      )[[1]]

    # evaluation errors are reported nicely
    roc_proc_text(rd_roclet(), "
      #' Title
      #'
      #' Description --`r 1 + 'a'`--
      #' @md
      #' @name dummy
      NULL")[[1]]

    # parse errors are reported nicely
    roc_proc_text(rd_roclet(), "
      #' Title
      #'
      #' Description --`r 1 + `--
      #' @md
      #' @name dummy
      NULL")[[1]]
  })
})

test_that("interleaving fences and inline code", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' @details Details `r x <- 10; x`
    #'
    #' ```{r}
    #' y <- x + 10
    #' y
    #' ```
    #'
    #' @md
    #' @name dummy
    NULL")[[1]]

  details <- out1$get_value("details")
  expect_match(details, "Details 10", fixed = TRUE)
  expect_match(details, "## [1] 20", fixed = TRUE)
})

test_that("fence options are used", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' @details Details
    #'
    #' ```{r eval = FALSE}
    #' this - would - fail - to - eval
    #' ```
    #'
    #' @md
    #' @name dummy
    NULL")[[1]]

  details <- out1$get_value("details")
  expect_false(grepl("Error", details))
})

test_that("dynamic code in fragile tags still runs", {
  out <- markdown("foo \\out{`r 1+1`} bar")
  expect_equal(out, "foo \\out{2} bar")
})

test_that("fragile tags in dynamic code are left alone", {
  out <- markdown("foo `r substr('\\\\out{xxx}', 2, 4)` bar")
  expect_equal(out, "foo out bar")
})

test_that("fragile tags in generated code", {
  out <- markdown("foo `r '\\\\out{*1*}'` bar")
  expect_equal(out, "foo \\out{*1*} bar")

  expect_silent(out2 <- markdown("foo `r '\\\\out{<span></span>}'` bar"))
  expect_equal(out2, "foo \\out{<span></span>} bar")
})
