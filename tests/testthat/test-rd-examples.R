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
  expect_match(examples, "example <- 'example1'", all = FALSE, fixed = TRUE)
  expect_match(examples, "example <- 'example2'", all = FALSE, fixed = TRUE)
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

  expect_equal(re_count(out$get_value("examples"), "\n"), 1L)
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
  expect_snapshot(out <- roc_proc_text(rd_roclet(), block))
  expect_equal(out[[1]]$get_section("examples"), NULL)
})

test_that("@examplesIf warns on empty body (#1695)", {
  block <- "
    #' @name a
    #' @title a
    #' @examplesIf interactive()
    NULL
  "
  expect_snapshot(out <- roc_proc_text(rd_roclet(), block))
  expect_equal(out[[1]]$get_section("examples"), NULL)
})

test_that("strings in R comments don't affect brace matching (#1492)", {
  block <- "
    #' @name a
    #' @title a
    #' @examples
    #' # {greeting}'
    NULL"

  expect_silent(out <- roc_proc_text(rd_roclet(), block)[[1]])
  expect_equal(out$get_value("examples"), rd("# {greeting}'"))
})

test_that("braces don't need to match inside of raw strings (#1492)", {
  block <- "
    #' @name a
    #' @title a
    #' @examples
    #' r'( '{{ )'
    NULL
  "
  expect_silent(out <- roc_proc_text(rd_roclet(), block)[[1]])
  expect_equal(out$get_value("examples"), rd("r'( '{{ )'"))
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

# strip_rd_example_tags -----------------------------------------------------

test_that("strip_rd_example_tags unwraps \\dontrun and \\donttest", {
  expect_equal(strip_rd_example_tags("\\dontrun{\n1 + 1\n}"), "1 + 1")
  expect_equal(strip_rd_example_tags("\\donttest{\n1 + 1\n}"), "1 + 1")
})

test_that("strip_rd_example_tags removes \\dontshow entirely", {
  expect_equal(strip_rd_example_tags("\\dontshow{\n1 + 1\n}"), "")
})

test_that("strip_rd_example_tags preserves surrounding code", {
  expect_equal(
    strip_rd_example_tags("x <- 1\n\\dontrun{\n1 + 1\n}\ny <- 2"),
    "x <- 1\n1 + 1\ny <- 2"
  )
})

test_that("strip_rd_example_tags handles nested tags", {
  expect_equal(
    strip_rd_example_tags("\\dontrun{\n\\dontshow{setup}\n1 + 1\n}"),
    "1 + 1"
  )
})

test_that("strip_rd_example_tags handles examplesIf wrappers", {
  input <- paste0(
    "\\dontshow{if (TRUE) withAutoprint(\\{ # examplesIf}\n",
    "1 + 1\n",
    "\\dontshow{\\}) # examplesIf}"
  )
  expect_equal(strip_rd_example_tags(input), "1 + 1")
})

test_that("strip_rd_example_tags is no-op with no tags", {
  expect_equal(strip_rd_example_tags("job$run()"), "job$run()")
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
