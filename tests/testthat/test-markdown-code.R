
test_that("can eval", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' @title Title `r 1 + 1`
    #' @description Description `r 2 + 2`
    #' @md
    foo <- function() {}")[[1]]

  expect_equal(out1$get_value("title"), "Title 2")
  expect_equal(out1$get_value("description"), "Description 4")
})

test_that("uses the same env for a tag, but does not reuse envs", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title `r foobarxxx123 <- 420` `r foobarxxx123`
    #'
    #' Description `r exists('foobarxxx123', inherits = FALSE)`
    #' @md
    #' @name dummy
    NULL")[[1]]
  expect_equal(out1$get_value("title"), "Title 420 420")
  expect_equal(out1$get_value("description"), "Description FALSE")
})

test_that("can create markdown markdup", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description `r paste0('_', 'keyword', '_')`
    #' @md
    #' @name dummy
    NULL")[[1]]
  expect_equal(out1$get_value("description"), "Description \\emph{keyword}")
})

test_that("can create markdown markup piecewise", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description [`r paste0('https://url]')`](`r paste0('link text')`).
    #' @md
    #' @name dummy
    NULL")[[1]]
  expect_equal(
    out1$get_value("description"),
    "Description \\link{https://url}](link text)."
  )
})

test_that("NULL creates no text", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description --`r NULL`--
    #' @md
    #' @name dummy
    NULL")[[1]]
  expect_equal(out1$get_value("description"), "Description ----")
})

test_that("multi-line inline code block errors", {
  expect_error(
    roc_proc_text(rd_roclet(), "
      #' Title
      #'
      #' Description --`r 1 +
      #'   1`--
      #' @md
      #' @name dummy
      NULL")[[1]],
    "in inline code: multi-line `r ` markup is not supported"
  )
})

test_that("evaluation errors are reported nicely", {
  expect_error(
    roc_proc_text(rd_roclet(), "
      #' Title
      #'
      #' Description --`r 1 + 'a'`--
      #' @md
      #' @name dummy
      NULL")[[1]],
    "in inline code: non-numeric argument to binary operator"
  )
})

test_that("pase errors are reported nicely", {
  expect_error(
    roc_proc_text(rd_roclet(), "
      #' Title
      #'
      #' Description --`r 1 + `--
      #' @md
      #' @name dummy
      NULL")[[1]],
    "in inline code:.*unexpected end of input"
  )
})
