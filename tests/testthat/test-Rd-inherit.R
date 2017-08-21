context("Rd: inherit")


# Rd parsing --------------------------------------------------------------

test_that("can round-trip Rd", {
  rd <- tools::parse_Rd(test_path("escapes.Rd"))

  field <- find_field(rd, "description")
  lines <- strsplit(field, "\n")[[1]]
  expect_equal(
    lines,
    c(
      "% Comment",   # Latex comments shouldn't be escaped
      "\\code{\\\\}" # Backslashes in code should be
    )
  )
})

# tag parsing -------------------------------------------------------------

test_that("warns on unknown inherit type", {
  expect_warning(
    parse_text("
      #' @inherit fun blah
      NULL
    "),
    "Unknown inherit type: blah"
  )
})

test_that("no options gives default values", {
  block <- parse_text("
    #' @inherit fun
    NULL
  ")[[1]]

  expect_equal(
    block$inherit$fields,
    c(
      "params", "return", "title", "description", "details", "seealso",
      "sections", "references", "examples"
    )
  )
})

test_that("some options overrides defaults", {
  block <- parse_text("
    #' @inherit fun return
    NULL
  ")[[1]]

  expect_equal(block$inherit$fields, "return")
})


# Inherit return values ---------------------------------------------------

test_that("can inherit return values from roxygen topic", {
  out <- roc_proc_text(rd_roclet(), "
    #' A.
    #'
    #' @return ABC
    a <- function(x) {}

    #' B
    #'
    #' @inherit a
    b <- function(y) {}
  ")[[2]]

  expect_equal(out$get_field("value")$values, "ABC")
})


test_that("takes value from first with return", {
  out <- roc_proc_text(rd_roclet(), "
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
  ")[[3]]

  expect_equal(out$get_field("value")$values, "B")
})

test_that("can inherit return value from external function", {
  out <- roc_proc_text(rd_roclet(), "
    #' A1
    #' @inherit base::mean
    a1 <- function(x) {}
  ")[[1]]

  expect_match(out$get_field("value")$values, "before the mean is computed.$")
  expect_match(out$get_field("value")$values, "^If \\\\code")
})


# Inherit seealso ---------------------------------------------------------

test_that("can inherit return values from roxygen topic", {
  out <- roc_proc_text(rd_roclet(), "
    #' A.
    #'
    #' @seealso ABC
    a <- function(x) {}

    #' B
    #'
    #' @inherit a
    b <- function(y) {}
  ")[[2]]

  expect_equal(out$get_field("seealso")$values, "ABC")
})

# Inherit description and details -----------------------------------------

test_that("can inherit description from roxygen topic", {
  out <- roc_proc_text(rd_roclet(), "
    #' A.
    #'
    #' B
    #'
    #' @return ABC
    a <- function(x) {}

    #' @title C
    #' @inherit a description
    b <- function(y) {}
  ")[[2]]

  expect_equal(out$get_field("description")$values, "B")
})

test_that("inherits description if omitted", {
  out <- roc_proc_text(rd_roclet(), "
    #' A.
    #'
    #' B
    #'
    #' @return ABC
    a <- function(x) {}

    #' C
    #' @inherit a description
    b <- function(y) {}
  ")[[2]]

  expect_equal(out$get_field("description")$values, "B")
})

test_that("can inherit details from roxygen topic", {
  out <- roc_proc_text(rd_roclet(), "
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
  ")[[2]]

  expect_equal(out$get_field("description")$values, "E")
  expect_equal(out$get_field("details")$values, "C")
})



# Inherit sections --------------------------------------------------------

test_that("inherits missing sections", {
    out <- roc_proc_text(rd_roclet(), "
    #' A.
    #' @section A:1
    #' @section B:1
    a <- function(x) {}

    #' D
    #'
    #' @section A:2
    #' @inherit a sections
    b <- function(y) {}
  ")[[2]]

  section <- out$get_field("section")
  expect_equal(section$title, c("A", "B"))
  expect_equal(section$content, c("2", "1"))
})

test_that("can inherit single section", {
    out <- roc_proc_text(rd_roclet(), "
    #' A.
    #' @section A:1
    #' @section B:1
    a <- function(x) {}

    #' D
    #'
    #' @inheritSection a B
    b <- function(y) {}
  ")[[2]]

  section <- out$get_field("section")
  expect_equal(section$title, "B")
  expect_equal(section$content, "1")
})


test_that("can find section in existing docs", {
  out <- find_sections(find_topic("base::attach"))

  expect_s3_class(out, "roxy_field_section")
  expect_equal(out$title, "Good practice")
})

# Inherit parameters ------------------------------------------------------

test_that("multiple @inheritParam tags gathers all params", {
  out <- roc_proc_text(rd_roclet(), "
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
    ")

  params <- get_tag(out[["c.Rd"]], "param")$values
  expect_equal(length(params), 2)

  expect_equal(params[["x"]], "X")
  expect_equal(params[["y"]], "Y")
})

test_that("@inheritParams can inherit from inherited params", {
  out <- roc_proc_text(rd_roclet(), "
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
    ")

  expect_equal(out[["c.Rd"]]$get_field("param")$values, c(x = "X"))
})

test_that("multiple @inheritParam inherits from existing topics", {
  out <- roc_proc_text(rd_roclet(), "
    #' My mean
    #'
    #' @inheritParams base::mean
    mymean <- function(x, trim) {}")[[1]]
  params <- get_tag(out, "param")$values
  expect_equal(length(params), 2)
  expect_equal(sort(names(params)), c("trim", "x"))
})


test_that("@inheritParam can cope with multivariable argument definitions", {
  out <- roc_proc_text(rd_roclet(), "
                       #' My merge
                       #'
                       #' @inheritParams base::merge
                       mymerge <- function(x, y) {}")[[1]]
  params <- get_tag(out, "param")$values
  expect_equal(length(params), 2)
  expect_equal(sort(names(params)), c("x", "y"))
})

test_that("@inheritParam understands compound docs", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' @param x x
    #' @param y x
    x <- function(x, y) {}

    #' Title
    #'
    #' @inheritParams x
    #' @param x y
    #' @param y y
    y <- function(x, y) {}")[[2]]
  params <- get_tag(out, "param")$values
  expect_equal(params, c(x = "y", y = "y"))
})


test_that("argument order, also for incomplete documentation", {
  out <- roc_proc_text(rd_roclet(), "
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
  ")

  expect_equal(get_tag(out[["a.Rd"]], "param")$values, c(x="X", y="Y"))
  expect_equal(get_tag(out[["b.Rd"]], "param")$values, c(y="Y"))
  expect_equal(get_tag(out[["c.Rd"]], "param")$values, c(x="X"))
  expect_equal(get_tag(out[["d.Rd"]], "param")$values, c(y="Y", z="Z"))
  expect_equal(get_tag(out[["e.Rd"]], "param")$values, c(x="X", y="Y"))
})

test_that("argument order with @inheritParam", {
  out <- roc_proc_text(rd_roclet(), "
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
    ")

  expect_equal(get_tag(out[["b1.Rd"]], "param")$values, c(x="X", y="B"))
  expect_equal(get_tag(out[["b2.Rd"]], "param")$values, c(x="X", y="B"))
  expect_equal(get_tag(out[["c1.Rd"]], "param")$values, c(x="C", y="Y"))
  expect_equal(get_tag(out[["c2.Rd"]], "param")$values, c(x="C", y="Y"))
})


test_that("inherit params ... named \\dots", {
  out <- roc_proc_text(rd_roclet(), "
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
  ")[[2]]

  expect_equal(
    out$get_field("param")$values,
    c(x = "x", "\\dots" = "bar")
  )

})

# inheritDotParams --------------------------------------------------------

test_that("can inherit all from single function", {
  out <- roc_proc_text(rd_roclet(), "
    #' Foo
    #'
    #' @param x x
    #' @param y y
    foo <- function(x, y) {}

    #' Bar
    #'
    #' @inheritDotParams foo
    bar <- function(...) {}
  ")[[2]]

  params <- out$get_field("param")$values
  expect_named(params, "...")
  expect_match(params, "Arguments passed on to \\code{foo}", fixed = TRUE)
  expect_match(params, "\\item{x}{x}", fixed = TRUE)
})

# inherit everything ------------------------------------------------------

test_that("can inherit all from single function", {
  out <- roc_proc_text(rd_roclet(), "
    #' Foo
    #'
    #' Description
    #'
    #' Details
    #'
    #' @param x x
    #' @param y y
    #' @examples
    #' x <- 1
    foo <- function(x, y) {}

    #' @inherit foo
    bar <- function(x, y) {}
  ")[[2]]

  params <- out$get_field("param")$values
  expect_named(params, c("x", "y"))
  expect_equal(out$get_field("title")$values, "Foo")
  expect_equal(out$get_field("description")$values, "Description")
  expect_equal(out$get_field("details")$values, "Details")
  expect_equal(out$get_field("examples")$values, rd("x <- 1"))
})
