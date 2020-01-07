
test_that("can eval in various tags", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' @title Title `r 1 + 1`
    #'
    #' @description Description `r 2 + 2`
    #'
    #' @details Details `r 3 + 3`
    #'
    #' @references References `r 4 + 4`
    #'
    #' @note Note `r 5 + 5`
    #'
    #' @seealso See also `r 6 + 6`
    #'
    #' @return Return `r 7 + 7`
    #'
    #' @author Author `r 8 + 8`
    #'
    #' @section Foobar `r 8.5 + 8.5`:
    #' With some `r 9 + 9`
    #'
    #' @format Format `r 10 + 10`
    #'
    #' @source Source `r 11 + 11`
    #'
    #' @param param Param `r 12 + 12`
    #'
    #' @slot slot Slot `r 13 + 13`
    #'
    #' @field field Field `r 14 + 14`
    #'
    #' @md
    foo <- function() {}")[[1]]

  expect_equal(out1$get_value("title"), "Title 2")
  expect_equal(out1$get_value("description"), "Description 4")
  expect_equal(out1$get_value("details"), "Details 6")
  expect_equal(out1$get_value("references"), "References 8")
  expect_equal(out1$get_value("note"), "Note 10")
  expect_equal(out1$get_value("seealso"), "See also 12")
  expect_equal(out1$get_value("value"), "Return 14")
  expect_equal(out1$get_value("author"), "Author 16")
  expect_equal(out1$get_value("section")$title, "Foobar 17")
  expect_equal(out1$get_value("section")$content, "\nWith some 18")
  expect_equal(out1$get_value("format"), "Format 20")
  expect_equal(out1$get_value("source"), "Source 22")
  expect_equal(out1$get_value("param"), c(param = "Param 24"))
  expect_equal(out1$get_value("slot"), c(slot = "Slot 26"))
  expect_equal(out1$get_value("field"), c(field = "Field 28"))
})

test_that("can see the package env", {
  # This is better tested in the internal `markdown_pass` manual
  # page, since there is a package env there, unlike here...
  expect_true(TRUE)
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
