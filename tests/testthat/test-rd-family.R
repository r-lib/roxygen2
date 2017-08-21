context("Rd: family")

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
  expect_true(grepl("^Other Long family name:", seealso))
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
  expect_true(grepl("^Other Long family name:", seealso))
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
  expect_true(grepl("^Other many aliases:", seealso))
  expect_equal(str_count(seealso, fixed("\\code{\\link")), 1)

})

test_that("families listed in same order as input", {
  out <- roc_proc_text(rd_roclet(), "
    #' foo
    #' @family a
    foo <- function() {}

    #' foo
    #' @family b
    #' @family a
    bar <- function() {}

    #' foo
    #' @family b
    baz <- function() {}
  ")[[2]]

  seealso <- get_tag(out, "seealso")$values
  expect_match(seealso[1], "^Other b")
  expect_match(seealso[2], "^Other a")
})

test_that("family also included in concepts", {
  out <- roc_proc_text(rd_roclet(), "
    #' foo
    #' @family a
    foo <- function() {}
  ")[[1]]

  expect_equal(out$get_field("concept")$values, "a")
})
