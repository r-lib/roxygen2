context("Family")
test_that("long families are wrapped", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #' @family Long family name
    #'
    long_function_name_________________________1 <- function() {}

    #' Title
    #' @family Long family name
    long_function_name_________________________2 <- function() {}

    #' Title
    #' @family Long family name
    #'
    long_function_name_________________________3 <- function() {}

    #' Title
    #' @family Long family name
    long_function_name_________________________4 <- function() {}
  ")[[1]]

  seealso <- get_tag(out, "seealso")$values
  expect_equal(str_count(seealso, "\n"), 2)

})

test_that("special names escaped in family tag", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #' @family Long family name
    f <- function() {}

    #' Title
    #' @family Long family name
    '%+%' <- function(a, b) {}
  ")[[1]]

  seealso <- get_tag(out, "seealso")$values
  expect_match(seealso, "\\\\%\\+\\\\%")

})

test_that("family links to name only, not all aliases", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #' @aliases f2 f3
    #'
    #' @family many aliases
    f <- function() {}

    #' Title
    #' @aliases g2 g3
    #'
    #' @family many aliases
    g <- function() {}
  ")[[1]]

  seealso <- get_tag(out, "seealso")$values
  expect_equal(str_count(seealso, fixed("\\code{\\link")), 1)

})
