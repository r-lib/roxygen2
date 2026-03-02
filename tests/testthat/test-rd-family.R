test_that("long families are wrapped", {
  out <- roc_proc_text(
    rd_roclet(),
    "
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
  "
  )[[1]]

  seealso <- out$get_value("seealso")
  expect_true(grepl("^Other Long family name:", seealso))
  expect_equal(str_count(seealso, "\n"), 3)
})

test_that("special names escaped in family tag", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #' @family Long family name
    f <- function() {}

    #' Title
    #' @family Long family name
    '%+%' <- function(a, b) {}
  "
  )[[1]]

  seealso <- out$get_value("seealso")
  expect_true(grepl("^Other Long family name:", seealso))
  expect_match(seealso, "\\\\%\\+\\\\%")
})

test_that("family links to name only, not all aliases", {
  out <- roc_proc_text(
    rd_roclet(),
    "
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
  "
  )[[1]]

  seealso <- out$get_value("seealso")
  expect_true(grepl("^Other many aliases:", seealso))
  expect_equal(str_count(seealso, fixed(r"(\code{\link)")), 1)
})

test_that("families listed in same order as input", {
  out <- roc_proc_text(
    rd_roclet(),
    "
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
  "
  )[[2]]

  seealso <- out$get_value("seealso")
  expect_match(seealso[1], "^Other b")
  expect_match(seealso[2], "^Other a")
})

test_that("only functions get () suffix", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' foo
    #' @family a
    foo <- function() {}

    #' bar
    #' @family a
    bar <- 1:10
  "
  )

  expect_equal(
    out[[1]]$get_value("seealso"),
    "Other a: \n\\code{\\link{bar}}"
  )
  expect_equal(
    out[[2]]$get_value("seealso"),
    "Other a: \n\\code{\\link{foo}()}"
  )
})

test_that("family also included in concepts", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' foo
    #' @family a
    foo <- function() {}
  "
  )[[1]]

  expect_equal(out$get_value("concept"), "a")
})

test_that("custom family prefixes can be set", {
  local_roxy_meta_set("rd_family_title", list(a = "Custom prefix: "))
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' foo
    #' @family a
    foo <- function() {}

    #' bar
    #' @family a
    bar <- function() {}
  "
  )[[1]]

  expect_match(out$get_value("seealso"), "^Custom prefix:")
})

test_that("custom family prefixes can include Markdown", {
  local_roxy_meta_set(
    "rd_family_title",
    list(a = "Custom ***strongly emphasized*** prefix: ")
  )
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' foo
    #' @family a
    foo <- function() {}

    #' bar
    #' @family a
    bar <- function() {}
  "
  )[[1]]

  expect_match(
    out$get_value("seealso"),
    "^Custom \\\\emph\\{\\\\strong\\{strongly emphasized\\}} prefix:"
  )
})

test_that("careful ordering", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' foo1
    #' @family a
    foo1 <- function() {}

    #' foo2
    #' @family a
    foo2 <- function() {}

    #' Foo3
    #' @family a
    Foo3 <- function() {}

    #' foo
    #' @family a
    foo <- function() {}
  "
  )

  expect_snapshot({
    out
  })
})
