
test_that("can eval", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' @title Title `r 1 + 1`
    #' @description Description `r 2 + 2`
    #' @md
    foo <- function() {}")[[1]]

  expect_equal(out1$get_value("title"), "Title 2")
  expect_equal(out1$get_value("description"), "Description 4")
})

test_that("uses the same env for a block, but not across blocks", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title `r foobarxxx123 <- 420` `r foobarxxx123`
    #'
    #' Description `r exists('foobarxxx123', inherits = FALSE)`
    #' @md
    #' @name dummy
    NULL

    #' Title another
    #'
    #' Description `r exists('foobarxxx123', inherits = FALSE)`
    #' @md
    #' @name dummy2
    NULL")
  expect_equal(out1$dummy.Rd$get_value("title"), "Title 420 420")
  expect_equal(out1$dummy.Rd$get_value("description"), "Description TRUE")
  expect_equal(out1$dummy2.Rd$get_value("description"), "Description FALSE")
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
      "Description [`r paste0('https://url]')`](`r paste0('link text')`)"
    ),
    "Description \\link{https://url}](link text)"
  )
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
    #' @details Details `r x <- 10`
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
