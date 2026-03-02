test_that("@example loads from specified files", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @name a
    #' @title a
    #'
    #' @example Rd-example-1.R
    #' @example Rd-example-2.R
    NULL"
  )[[1]]

  examples <- out$get_value("examples")
  expect_match(examples, fixed("example <- 'example1'"), all = FALSE)
  expect_match(examples, fixed("example <- 'example2'"), all = FALSE)
})

test_that("@example captures examples (#470)", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @name a
    #' @title a
    #' @examples
    #' TRUE
    #' @examples
    #' FALSE
    NULL"
  )[[1]]

  examples <- out$get_value("examples")
  expect_equal(examples, rd(c("TRUE", "FALSE")))
})

test_that("@examples and @example interleave", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @name a
    #' @title a
    #' @example Rd-example-1.R
    #' @examples a <- 2
    #' @example Rd-example-2.R
    NULL"
  )[[1]]

  expect_snapshot_output(out$get_section("examples"))
})

test_that("@example does not introduce extra empty lines", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @name a
    #' @title a
    #' @example Rd-example-3.R
    NULL"
  )[[1]]

  expect_equal(str_count(out$get_value("examples"), "\n"), 1L)
})

test_that("@example gives warning if used instead of @examples", {
  block <- "
    #' @name a
    #' @title a
    #' @example
    #' a <- 1
    #' a + b
    NULL
  "
  expect_snapshot(out <- roc_proc_text(rd_roclet(), block)[[1]])
  expect_null(out$get_value("examples"))
})

test_that("warns if path doesn't exist", {
  block <- "
    #' @name a
    #' @title a
    #' @example this-path-doesnt-exist.R
    NULL
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))
})

test_that("@examplesIf", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @name a
    #' @title a
    #' @examplesIf foo::bar()
    #' maybe-run-this-code
    #' @examplesIf foobar()
    #' and-this
    #' and-that
    NULL"
  )[[1]]

  expect_snapshot_output(out$get_section("examples"))
})

test_that("@examplesIf warns about unparseable condition", {
  block <- "
    #' @name a
    #' @title a
    #' @examplesIf 1 +
    #' maybe-run-this-code
    NULL
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))
})

test_that("% in @examples escaped before matching braces test (#213)", {
  block <- "
    #' @name a
    #' @title a
    #' @examples
    #' {a %% b}
    NULL
  "
  out <- roc_proc_text(rd_roclet(), block)[[1]]
  expect_equal(out$get_value("examples"), rd("{a \\%\\% b}"))
})

# escapes ------------------------------------------------------------------

test_that("only % escaped in @examples", {
  expect_equal(escape_examples("x %*% y"), rd("x \\%*\\% y"))
  expect_equal(escape_examples("# \\x"), rd("# \\x"))
  expect_equal(escape_examples("'34.00\\'"), rd("'34.00\\'"))
})

test_that("multi-line macros in @example", {
  # https://github.com/r-lib/roxygen2/issues/974
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @name a
    #' @title a
    #'
    #' @example Rd-example-4.txt
    NULL"
  )[[1]]

  expect_equal(
    format(out$get_section("examples")),
    "\\examples{\n\\dontrun{\n1 + 1\n}\n}"
  )
})
