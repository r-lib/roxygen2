test_that("can eval inline code", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "

    #' @title Title `r 1 + 1`
    #' @description Description `r 2 + 2`
    #' @md
    foo <- function() NULL

  "
  )[[1]]
  expect_equal(out1$get_value("title"), "Title 2")
  expect_equal(out1$get_value("description"), "Description 4")
})

test_that("can eval fenced code", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "

    #' @title Title
    #' @details Details
    #' ```{r lorem}
    #' 1+1
    #' ```
    #' @md
    foo <- function() NULL

  "
  )[[1]]
  expect_match(out1$get_value("details"), "2")
})

test_that("use same env within, but not across blocks", {
  example <- "
    #' Title `r baz <- 420` `r baz`
    #'
    #' Description `r exists('baz', inherits = FALSE)`
    #' @md
    bar <- function() NULL

    #' Title
    #'
    #' Description `r exists('baz', inherits = FALSE)`
    #' @md
    zap <- function() NULL
  "
  out1 <- roc_proc_text(rd_roclet(), example)[[1]]
  out2 <- roc_proc_text(rd_roclet(), example)[[2]]
  expect_equal(out1$get_value("title"), "Title  420")
  expect_equal(out1$get_value("description"), "Description TRUE")
  expect_equal(out2$get_value("description"), "Description FALSE")
})

test_that("appropriate knit print method for fenced and inline is applied", {
  rlang::local_bindings(
    knit_print.foo = function(x, inline = FALSE, ...) {
      knitr::asis_output(ifelse(inline, "inline", "fenced"))
    },
    .env = globalenv()
  )
  out1 <- roc_proc_text(
    rd_roclet(),
    "
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
  "
  )
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
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #' Description `r paste0('\\x60', 'bar', '\\x60')`
    #' @md
    foo <- function() NULL
  "
  )[[1]]
  expect_match(out1$get_value("title"), "\\code{bar}", fixed = TRUE)
})

test_that("NULL creates no text", {
  expect_identical(
    markdown("Description --`r NULL`--"),
    "Description ----"
  )
})


test_that("multi-line inline code gives useful warning", {
  block <- "
    #' Title
    #'
    #' `r 1 +
    #' 1`
    #' @md
    foo <- function() {}
  "

  expect_snapshot(
    out <- roc_proc_text(rd_roclet(), block)[[1]]
  )
  expect_equal(out$get_value("description"), r"(\verb{r 1 + 1})")
})

test_that("inline code gives useful warning", {
  block <- "
    #' Title
    #'
    #' `r 1 + `
    #' @md
    foo <- function() {}
  "

  expect_snapshot(
    out <- roc_proc_text(rd_roclet(), block)[[1]],
    transform = function(x) {
      line <- grep("~~~", x)[1]
      if (!is.na(line)) {
        x <- x[1:(line - 1)]
      }
      x
    }
  )
  expect_equal(out$get_value("description"), r"(\verb{r 1 + })")
})

test_that("interleaving fences and inline code", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
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
    NULL"
  )[[1]]

  expect_snapshot(cat(out1$get_value("details")))
})

test_that("preserves white space", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' @details
    #'
    #' ```{r}
    #' a <- 1
    #'
    #' b <- 2
    #' ```
    #'
    #' ```{r}
    #' c <- 3
    #' ```
    #'
    #' @md
    #' @name dummy
    NULL"
  )[[1]]

  expect_snapshot(cat(out1$get_value("details")))
})

test_that("fence options are used", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
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
    NULL"
  )[[1]]

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

test_that("workaround for cmark sourcepos bug (#1353) works", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' line1
    #'    pre `r \"1\"` 2 `r 1+2` post
    #'
    #' no workaround needed `r 'here'`
    #' @md
    foo <- function() {}
  "
  )[[1]]

  expect_equal(out$get_section("description")$value, "line1\npre 1 2 3 post")
  expect_equal(out$get_section("details")$value, "no workaround needed here")
})


test_that("doesn't generate NA language", {
  out <- roc_proc_text(
    rd_roclet(),
    "
  #' Title
  #'
  #' ```
  #' r <- 1:10
  #' ```
  #' @md
  foo <- function() {}"
  )[[1]]
  expect_false(grepl("NA", out$get_section("description")$value))
})
