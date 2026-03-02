test_that("invalid syntax generates useful warning", {
  block <- "
    #' A
    #' @templateVar
    a <- function() {}
  "

  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))
})

test_that("can find template from name", {
  base <- test_path("templates/")

  expect_equal(
    template_find(base, "UCase"),
    file.path(base, "man-roxygen", "UCase.R")
  )

  # On case-insensitive file systems, will find upper case version first
  expect_equal(
    tolower(template_find(base, "lcase")),
    tolower(file.path(base, "man-roxygen", "lcase.r"))
  )

  expect_equal(
    template_find(base, "new-path"),
    file.path(base, "man", "roxygen", "templates", "new-path.R")
  )

  expect_error(
    template_find(base, "missing"),
    "Can't find template"
  )
})

test_that("templates gives useful error if not found", {
  block <- "
    #' @template doesn't-exist
    x <- 10
  "
  expect_snapshot(roc_proc_text(rd_roclet(), block), error = TRUE)
})

test_that("templates replace variables with their values", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @template values
    #' @templateVar x a
    #' @templateVar y b
    #' @templateVar z c
    x <- 10"
  )[[1]]

  expect_equal(out$get_value("title"), "a")
  expect_equal(out$get_value("param"), c(b = "c"))
})
