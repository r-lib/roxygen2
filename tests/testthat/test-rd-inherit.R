# Rd parsing --------------------------------------------------------------

test_that("can round-trip Rd", {
  rd <- tools::parse_Rd(test_path("escapes.Rd"))

  field <- find_field(rd, "description")
  lines <- strsplit(field, "\n")[[1]]
  expect_equal(
    lines,
    c(
      "% Comment", # Latex comments shouldn't be escaped
      "\\code{\\\\}" # Backslashes in code should be
    )
  )
})

test_that("\\links are transformed", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' @inheritParams digest::sha1
    wrapper <- function(algo) {}"
  )[[1]]

  # \\link{} should include [digest]
  expect_snapshot_output(out$get_section("param"))
})

test_that("markdown doesn't get get extra parens", {
  expect_equal(rd2text(parse_rd("\\href{a}{b}")), "\\href{a}{b}\n")
  expect_equal(rd2text(parse_rd("\\ifelse{a}{b}{c}")), "\\ifelse{a}{b}{c}\n")
  expect_equal(rd2text(parse_rd("\\if{a}{b}")), "\\if{a}{b}\n")
})

test_that("relative links converted to absolute", {
  link_to_base <- function(x) {
    rd2text(parse_rd(x), package = "base")
  }

  expect_equal(
    link_to_base("\\link{abbreviate}"),
    "\\link[base]{abbreviate}\n"
  )
  expect_equal(
    link_to_base("\\link[=abbreviate]{abbr}"),
    "\\link[base:abbreviate]{abbr}\n"
  )

  # Doesn't affect links that already have
  expect_equal(
    link_to_base("\\link[foo]{abbreviate}"),
    "\\link[foo]{abbreviate}\n"
  )
  expect_equal(
    link_to_base("\\link[foo::abbreviate]{abbr}"),
    "\\link[foo::abbreviate]{abbr}\n"
  )
})

# tag parsing -------------------------------------------------------------

test_that("invalid syntax gives useful warning", {
  block <- "
    #' @inheritDotParams
    #' @inheritSection
    NULL
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))
})

test_that("warns on unknown inherit type", {
  text <- "
    #' @inherit fun blah
    NULL
  "
  expect_snapshot(parse_text(text))
})

test_that("no options gives default values", {
  block <- parse_text(
    "
    #' @inherit fun
    NULL
  "
  )[[1]]

  expect_equal(block_get_tag_value(block, "inherit")$fields, inherit_components)
})

test_that("some options overrides defaults", {
  block <- parse_text(
    "
    #' @inherit fun return
    NULL
  "
  )[[1]]

  expect_equal(block_get_tag_value(block, "inherit")$fields, "return")
})


# Inherit return values ---------------------------------------------------

test_that("can inherit return values from roxygen topic", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A.
    #'
    #' @return ABC
    a <- function(x) {}

    #' B
    #'
    #' @inherit a
    b <- function(y) {}
  "
  )[[2]]

  expect_equal(out$get_value("value"), "ABC")
})


test_that("takes value from first with return", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A1
    #' @return A
    a1 <- function(x) {}

    #' A2
    a2 <- function() {}

    #' B
    #' @return B
    b <- function(x) {}

    #' C
    #' @inherit a2
    #' @inherit b
    #' @inherit a1
    c <- function(y) {}
  "
  )[[3]]

  expect_equal(out$get_value("value"), "B")
})

test_that("can inherit return value from external function", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A1
    #' @inherit base::mean
    a1 <- function(x) {}
  "
  )[[1]]

  expect_match(out$get_value("value"), "before the mean is computed.$")
  expect_match(out$get_value("value"), "^If \\\\code")
})


# Inherit seealso ---------------------------------------------------------

test_that("can inherit return values from roxygen topic", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A.
    #'
    #' @seealso ABC
    a <- function(x) {}

    #' B
    #'
    #' @inherit a
    b <- function(y) {}
  "
  )[[2]]

  expect_equal(out$get_value("seealso"), "ABC")
})

# Inherit description and details -----------------------------------------

test_that("can inherit description from roxygen topic", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A.
    #'
    #' B
    #'
    #' @return ABC
    a <- function(x) {}

    #' @title C
    #' @inherit a description
    b <- function(y) {}
  "
  )[[2]]

  expect_equal(out$get_value("description"), "B")
})

test_that("inherits description if omitted", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A.
    #'
    #' B
    #'
    #' @return ABC
    a <- function(x) {}

    #' C
    #' @inherit a description
    b <- function(y) {}
  "
  )[[2]]

  expect_equal(out$get_value("description"), "B")
})

test_that("can inherit details from roxygen topic", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A.
    #'
    #' B
    #'
    #' C
    #'
    #' @return ABC
    a <- function(x) {}

    #' D
    #'
    #' E
    #'
    #' @inherit a details
    b <- function(y) {}
  "
  )[[2]]

  expect_equal(out$get_value("description"), "E")
  expect_equal(out$get_value("details"), "C")
})


# Inherit sections --------------------------------------------------------

test_that("inherits missing sections", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A.
    #' @section A:1
    #' @section B:1
    a <- function(x) {}

    #' D
    #'
    #' @section A:2
    #' @inherit a sections
    b <- function(y) {}
  "
  )[[2]]

  section <- out$get_value("section")
  expect_equal(section$title, c("A", "B"))
  expect_equal(section$content, c("2", "1"))
})

test_that("can inherit single section", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A.
    #' @section A:1
    #' @section B:1
    a <- function(x) {}

    #' D
    #'
    #' @inheritSection a B
    b <- function(y) {}
  "
  )[[2]]

  section <- out$get_value("section")
  expect_equal(section$title, "B")
  expect_equal(section$content, "1")
})


test_that("warns if can't find section", {
  code <- "
    #' a
    a <- function(x) {}

    #' b
    #'
    #' @inheritSection a A
    b <- function(y) {}
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), code))
})

# Inherit parameters ------------------------------------------------------

test_that("match_params can ignore . prefix", {
  expect_equal(match_param("a", c("x", "y", "z")), NULL)
  expect_equal(match_param("x", c("x", "y", "z")), "x")
  expect_equal(match_param(".x", c("x", "y", "z")), "x")
  expect_equal(match_param("x", c(".x", ".y", ".z")), ".x")
  expect_equal(match_param(".x", c(".x", ".y", ".z")), ".x")
  expect_equal(match_param(c(".x", "y"), c(".x", ".y", ".z")), c(".x", ".y"))
  expect_equal(match_param(c(".x", "x"), c("x", ".x")), c(".x", "x"))
  expect_equal(match_param(c(".x", "x"), "x"), "x")
  expect_equal(match_param("x", c(".x", "x")), c("x", ".x"))
})

test_that("multiple @inheritParam tags gathers all params", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A.
    #'
    #' @param x X
    a <- function(x) {}

    #' B
    #'
    #' @param y Y
    b <- function(y) {}

    #' C
    #'
    #' @inheritParams a
    #' @inheritParams b
    c <- function(x, y) {}
    "
  )

  params <- out[["c.Rd"]]$get_value("param")
  expect_equal(length(params), 2)

  expect_equal(params[["x"]], "X")
  expect_equal(params[["y"]], "Y")
})

test_that("multiple @inheritParam tags gathers all params", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A.
    #'
    #' @param x X
    a <- function(x) {}

    #' B
    #'
    #' @param .y Y
    b <- function(.y) {}

    #' C
    #'
    #' @inheritParams a
    #' @inheritParams b
    c <- function(.x, y) {}
    "
  )[[3]]
  expect_equal(out$get_value("param"), c(.x = "X", y = "Y"))
})

test_that("@inheritParam preserves mixed names", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A.
    #' @param .x,x X
    a <- function(x, .x) {}

    #' B
    #' @inheritParams a
    b <- function(x, .x) {}
  "
  )[[2]]

  expect_equal(out$get_value("param"), c(".x,x" = "X"))
})

test_that("can inherit from same arg twice", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A.
    #'
    #' @param x X
    a <- function(x) {}

    #' B
    #'
    #' @inheritParams a
    b <- function(x) {}

    #' C
    #'
    #' @inheritParams a
    #' @rdname b
    c <- function(.x) {}
    "
  )[[2]]
  expect_equal(out$get_value("param"), c("x,.x" = "X"))
})

test_that("@inheritParams can inherit from inherited params", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' C
    #'
    #' @inheritParams b
    c <- function(x) {}

    #' B
    #'
    #' @inheritParams a
    b <- function(x) {}

    #' A.
    #'
    #' @param x X
    a <- function(x) {}
    "
  )

  expect_equal(out[["c.Rd"]]$get_value("param"), c(x = "X"))
})

test_that("multiple @inheritParam inherits from existing topics", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' My mean
    #'
    #' @inheritParams base::mean
    mymean <- function(x, trim) {}"
  )[[1]]
  params <- out$get_value("param")
  expect_equal(length(params), 2)
  expect_equal(sort(names(params)), c("trim", "x"))
})


test_that("@inheritParam can inherit multivariable arguments", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A
    #' @param x,y X and Y
    A <- function(x, y) {}

    #' B
    #'
    #' @inheritParams A
    B <- function(x, y) {}"
  )[[2]]
  expect_equal(out$get_value("param"), c("x,y" = "X and Y"))

  # Even when the names only match without .
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A
    #' @param x,y X and Y
    A <- function(x, y) {}

    #' B
    #'
    #' @inheritParams A
    B <- function(.x, .y) {}"
  )[[2]]
  expect_equal(out$get_value("param"), c(".x,.y" = "X and Y"))
})

test_that("@inheritParam only inherits exact multiparam matches", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A
    #' @param x,y X and Y
    A <- function(x, y) {}

    #' B
    #'
    #' @inheritParams A
    B <- function(x) {}"
  )[[2]]
  expect_equal(out$get_value("param"), NULL)
})


test_that("@inheritParam understands compound docs", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' @param x x
    #' @param y x
    x <- function(x, y) {}

    #' Title
    #'
    #' @inheritParams x
    #' @param y y
    y <- function(x, y) {}"
  )[[2]]
  params <- out$get_value("param")
  expect_equal(params, c(x = "x", y = "y"))
})

test_that("warned if no params need documentation", {
  code <- "
    #' Title
    #'
    #' @param x x
    #' @param y x
    #' @inheritParams foo
    x <- function(x, y) {}
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), code))
})

test_that("argument order, also for incomplete documentation", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A.
    #'
    #' @param y Y
    #' @param x X
    a <- function(x, y) {}

    #' B
    #'
    #' @param y Y
    b <- function(x, y) {}

    #' C
    #'
    #' @param x X
    c <- function(x, y) {}

    #' D
    #'
    #' @inheritParams b
    #' @param z Z
    d <- function(x, y, z) {}

    #' E
    #'
    #' @inheritParams c
    #' @param y Y
    e <- function(x, y, z) {}
  "
  )

  expect_equal(out[["a.Rd"]]$get_value("param"), c(x = "X", y = "Y"))
  expect_equal(out[["b.Rd"]]$get_value("param"), c(y = "Y"))
  expect_equal(out[["c.Rd"]]$get_value("param"), c(x = "X"))
  expect_equal(out[["d.Rd"]]$get_value("param"), c(y = "Y", z = "Z"))
  expect_equal(out[["e.Rd"]]$get_value("param"), c(x = "X", y = "Y"))
})

test_that("argument order with @inheritParam", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A.
    #'
    #' @param x X
    #' @param y Y
    a <- function(x, y) {}

    #' B1
    #'
    #' @param y B
    #' @inheritParams a
    b1 <- function(x, y) {}

    #' B2
    #'
    #' @inheritParams a
    #' @param y B
    b2 <- function(x, y) {}

    #' C1
    #'
    #' @param x C
    #' @inheritParams a
    c1 <- function(x, y) {}

    #' C2
    #'
    #' @inheritParams a
    #' @param x C
    c2<- function(x, y) {}
    "
  )

  expect_equal(out[["b1.Rd"]]$get_value("param"), c(x = "X", y = "B"))
  expect_equal(out[["b2.Rd"]]$get_value("param"), c(x = "X", y = "B"))
  expect_equal(out[["c1.Rd"]]$get_value("param"), c(x = "C", y = "Y"))
  expect_equal(out[["c2.Rd"]]$get_value("param"), c(x = "C", y = "Y"))
})


test_that("inherit params ... named \\dots", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Foo
    #'
    #' @param x x
    #' @param \\dots foo
    foo <- function(x, ...) {}

    #' Bar
    #'
    #' @inheritParams foo
    #' @param \\dots bar
    bar <- function(x=1, ...) {}
  "
  )[[2]]

  expect_equal(
    out$get_value("param"),
    c(x = "x", "\\dots" = "bar")
  )
})

# inheritDotParams --------------------------------------------------------

test_that("can inherit all from single function", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Foo
    #'
    #' @param x x
    #' @param y y
    foo <- function(x, y) {}

    #' Bar
    #'
    #' @inheritDotParams foo
    bar <- function(...) {}
  "
  )[[2]]

  expect_snapshot_output(test_path("test-rd-inherit-dots.txt"))
})

test_that("does not produce multiple ... args", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Foo
    #'
    #' @inheritParams bar
    #' @inheritDotParams baz
    foo <- function(x, ...) {}

    #' Bar
    #'
    #' @param x x
    #' @param ... dots
    bar <- function(x, ...) {}

    #' Baz
    #'
    #' @param y y
    #' @param z z
    baz <- function(y, z) {}
  "
  )[[1]]

  expect_snapshot_output(test_path("test-rd-inherit-dots-inherit.txt"))
})

test_that("can inherit dots from several functions", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Foo
    #'
    #' @param x x
    #' @param y y1
    foo <- function(x, y) {}

    #' Bar
    #'
    #' @param y y2
    #' @param z z
    bar <- function(z) {}

    #' Foobar
    #'
    #' @inheritDotParams foo
    #' @inheritDotParams bar
    foobar <- function(...) {}
  "
  )[[3]]

  expect_snapshot_output(out$get_section("param"))
})

test_that("inheritDotParams does not add already-documented params", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Wrapper around original
    #'
    #' @inherit original
    #' @inheritDotParams original
    #' @param y some more specific description
    #' @export
    wrapper <- function(x = 'some_value', y = 'some other value', ...) {
      original(x = x, y = y, ...)
    }

    #' Original function
    #'
    #' @param x x description
    #' @param y y description
    #' @param z z description
    #' @export
    original <- function(x, y, z, ...) {}
  "
  )[[1]]

  params <- out$get_value("param")
  dot_param <- params[["..."]]
  expect_named(params, c("x", "y", "..."))
  expect_false(grepl("item{x}{x description}", dot_param, fixed = TRUE))
  expect_false(grepl("item{y}{y description}", dot_param, fixed = TRUE))
  expect_match(dot_param, "item{\\code{z}}{z description}", fixed = TRUE)
})

test_that("useful error for bad inherits", {
  text <- "
    #' Foo
    #'
    #' @param x x
    #' @param y y
    foo <- function(x, y) {}

    #' Bar
    #'
    #' @inheritDotParams foo -z
    bar <- function(...) {}
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), text))
})


# inherit everything ------------------------------------------------------

test_that("can inherit all from single function", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Foo
    #'
    #' Description
    #'
    #' Details
    #'
    #' @param x x
    #' @param y y
    #' @author Hadley
    #' @source my mind
    #' @note my note
    #' @format my format
    #' @examples
    #' x <- 1
    foo <- function(x, y) {}

    #' @inherit foo
    bar <- function(x, y) {}
  "
  )[[2]]

  expect_named(out$get_value("param"), c("x", "y"))
  expect_equal(out$get_value("title"), "Foo")
  expect_equal(out$get_value("description"), "Description")
  expect_equal(out$get_value("details"), "Details")
  expect_equal(out$get_value("examples"), rd("x <- 1"))
  expect_equal(out$get_value("author"), "Hadley")
  expect_equal(out$get_value("source"), "my mind")
  expect_equal(out$get_value("format"), "my format")
  expect_equal(out$get_value("note"), "my note")
})


# get_rd() -----------------------------------------------------------------

test_that("useful warnings if can't find topics", {
  expect_snapshot({
    get_rd("base2::attach", source = "source")
    get_rd("base::function_not_found", source = "source")
    get_rd("function", RoxyTopics$new(), source = "source")
    get_rd("foo::bar()", RoxyTopics$new(), source = "source")
  })
})

test_that("can find section in existing docs", {
  out <- find_sections(get_rd("base::attach"))
  expect_equal(out$title, "Good practice")
})
