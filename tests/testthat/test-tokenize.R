# tokenise_block ----------------------------------------------------------

test_that("parses into tag and value", {
  x <- tokenise_block("#' @xyz abc", file = "", offset = 0)
  expect_equal(length(x), 1)

  expect_equal(x[[1]]$tag, "xyz")
  expect_equal(x[[1]]$raw, "abc")
})

test_that("description block gets empty tag", {
  x <- tokenise_block("#' abc", file = "", offset = 0L)
  expect_equal(length(x), 1)

  expect_equal(x[[1]]$tag, "")
  expect_equal(x[[1]]$raw, "abc")
})

test_that("multi line tags collapsed into one", {
  x <- tokenise_block(
    c(
      "#' @tag abc",
      "#'   def"
    ),
    file = "",
    offset = 0L
  )
  expect_equal(length(x), 1)
  expect_equal(x[[1]]$raw, "abc\n  def")
})

test_that("description block gets empty tag when followed by tag", {
  x <- tokenise_block(
    c(
      "#' abc",
      "#' @xyz abc"
    ),
    file = "",
    offset = 0L
  )
  expect_equal(length(x), 2)

  expect_equal(x[[1]]$tag, "")
  expect_equal(x[[1]]$raw, "abc")

  expect_equal(x[[2]]$tag, "xyz")
  expect_equal(x[[2]]$raw, "abc")
})

test_that("leading whitespace is ignored", {
  ref <- tokenise_block("#' abc", file = "", offset = 0L)

  expect_equal(tokenise_block("   #' abc", file = "", offset = 0L), ref)
})

test_that("need one or more #", {
  ref <- tokenise_block("#' abc", file = "", offset = 0L)

  expect_equal(tokenise_block("##' abc", file = "", offset = 0L), ref)
  expect_equal(tokenise_block("###' abc", file = "", offset = 0L), ref)
})

test_that("@@ becomes @", {
  expect_equal(
    tokenise_block("#' @tag @@", file = "", offset = 0L)[[1]]$raw,
    "@"
  )
})

# Inline comments ---------------------------------------------------------

test_that("Inline comments are supported", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Description
    a <- function(x) {
      #' @param x an integer
      stopifnot(is.integer(x))
    }"
  )[[1]]
  expect_equal(out$get_value("param"), c(x = "an integer"))
})

test_that("Inline comments just before the closing brace are allowed", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Description
    a <- function(x) {
      #' @param x an integer
      stopifnot(is.integer(x))

      #' @seealso somewhere
    }"
  )[[1]]
  expect_equal(out$get_value("seealso"), "somewhere")
})

test_that("Inline comments do not extend past the closing brace", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Description
    a <- function(x) {
      #' @param x an integer
      stopifnot(is.integer(x))
    }; #' @seealso somewhere"
  )[[1]]
  expect_null(out$get_value("seealso"))
})

test_that("Line numbers are ok", {
  check_line_nums <- function(block, lines) {
    for (t in names(lines)) {
      expect_equal(block_get_tag(block, t)$line, lines[[t]], info = t)
    }
  }

  text <-
    "#' @title
     #' Foo
     #'
     #' @description
     #' Description
     #'
     #' @details
     #' Details
     #'
     #' @param x xyz
     #' @export
     NULL"
  block <- parse_text(text)[[1]]
  ls <- c(title = 1, description = 4, details = 7, param = 10, export = 11)
  check_line_nums(block, ls)

  text <-
    "#' @title Foo
     #'
     #' @description Description
     #'
     #' @details Details
     #'
     #' @param x xy
     #' z
     #'
     #' @export
     NULL"
  block <- parse_text(text)[[1]]
  ls <- c(title = 1, description = 3, details = 5, param = 7, export = 10)
  check_line_nums(block, ls)

  text <-
    "#' @title Foo
     #'
     #' @description Description
     #'
     #' @details Details
     # not - a - roxy - comment
     #' @param x xy
     #' z
     quote(neither - is -
     #' @export
     this)"
  block <- parse_text(text)[[1]]
  ls <- c(title = 1, description = 3, details = 5, param = 7, export = 10)
  check_line_nums(block, ls)

  text <-
    "# 1
     # 2
     #' foo
     #'
     #' Description
     # 6
     # 7
     #' @param x xyz
     NULL"
  block <- parse_text(text)[[1]]
  ls <- c(title = 3, description = 5, param = 8)
  check_line_nums(block, ls)
})
