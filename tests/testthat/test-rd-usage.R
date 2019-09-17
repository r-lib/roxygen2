context("Rd: usage")


# @usage -----------------------------------------------------------------------

test_that("@usage overrides default", {
  out <- roc_proc_text(rd_roclet(), "
    #' A
    #' @usage a(a=2)
    a <- function(a=1) {}")[[1]]
  expect_equal(get_tag(out, "usage")$values, rd("a(a=2)"))
})

test_that("@usage overrides default for @docType data", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    #'
    #' @name abc
    #' @docType data
    #' @usage data(abc)
    NULL")[[1]]

  expect_equal(get_tag(out, "usage")$values, rd("data(abc)"))
})

test_that("@usage NULL suppresses default usage", {
  out <- roc_proc_text(rd_roclet(), "
    #' A
    #' @usage NULL
    a <- function(a=1) {}")[[1]]

  expect_equal(get_tag(out, "usage")$values, NULL)
})

test_that("quoted topics have usage statements", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    \"f\" <- function(a = 1, b = 2, c = a + b) {}")[[1]]

  expect_equal(get_tag(out, "usage")$values,
    rd("f(a = 1, b = 2, c = a + b)"))

  expect_equal(format(get_tag(out, "usage")),
    "\\usage{\nf(a = 1, b = 2, c = a + b)\n}"
  )

})

# Escaping --------------------------------------------------------------------

test_that("usage escaping preserved when combined", {
  out <- roc_proc_text(rd_roclet(), "
    #' Foo
    foo <- function(x = '%') x

    #' @rdname foo
    bar <- function(y = '%') y
  ")[[1]]

  expect_is(get_tag(out, "usage")$values, "rd")
})

test_that("default usage not double escaped", {
  out <- roc_proc_text(rd_roclet(), "
    #' Regular
    mean.foo <- function(x) 'foo'
  ")[[1]]

  expect_equal(format(get_tag(out, "usage")),
    "\\usage{\n\\method{mean}{foo}(x)\n}")
})

test_that("% and \\ are escaped in usage", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    a <- function(a='%\\\\') {}")[[1]]
  expect_equal(get_tag(out, "usage")$values, escape('a(a = "%\\\\")'))
  expect_equal(format(get_tag(out, "usage")),
    "\\usage{\na(a = \"\\%\\\\\\\\\")\n}")
})

test_that("% and \\ not escaped in manual usage", {
  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    #' @usage %\\
    a <- function(a) {}
  ")[[1]]
  expect_equal(get_tag(out, "usage")$values, rd('%\\'))
  expect_equal(format(get_tag(out, "usage")), '\\usage{\n%\\\n}')
})

test_that("non-syntactic names are quoted", {

  out <- roc_proc_text(rd_roclet(), "
    #' Title.
    'a b' <- function(x) x")[[1]]

  expect_equal(get_tag(out, "usage")$values, rd('"a b"(x)'))
})


test_that("Special vars removed in rc methods usage", {
  out <- roc_proc_text(rd_roclet(), "
    #' Class Blob
    ABCD <- setRefClass('ABC', methods = list(
      draw = function(x = 1) {
        \"2\"
        x
      })
    )
  ")[[1]]

  methods <- get_tag(out, "rcmethods")$values
  expect_equal(methods, list("draw(x = 1)" = "2"))
})

# Wrapping --------------------------------------------------------------------

test_that("long usages protected from incorrect breakage", {
  out <- roc_proc_text(rd_roclet(), "
    #' Function long usage
    f <- function(a = '                                    a',
                  b = '                                    b',
                  c = '                                    c',
                  d = '                                    d') 1")[[1]]

  usage <- format(get_tag(out, "usage"))
  expect_equal(str_count(usage, "\n"), 5)
})

test_that("argument vectors split on whitespace", {
  out <- roc_proc_text(rd_roclet(), "
    #' Function long usage
    f <- function(a = c('abcdef', 'abcdef', 'abcdef', 'abcdef', 'abcdef',
                  'abcdef', 'abcdef', 'abcdef'))  1")[[1]]

  usage <- get_tag(out, "usage")[[2]]
  expect_equal(str_count(usage, "\n"), 1)
})

test_that("\\method not split inappropriately", {
  out <- roc_proc_text(rd_roclet(), "
    #' Function long usage
    mean.reallyratherquitelongclassname <-
      function(reallyquitelongargument = 'reallyratherquitelongvalue') 1
  ")[[1]]
  usage <- format(get_tag(out, "usage"))
  expect_match(usage, "\\{mean\\}\\{reallyratherquitelongclassname\\}")
})

test_that("long usages protected from incorrect breakage", {
  out <- roc_proc_text(rd_roclet(), "
    #' Function long usage
    f <- function(a = '                                    a',
    b = '                                    b',
    c = '                                    c',
    d = '                                    d') 1")[[1]]

  usage <- format(get_tag(out, "usage"))
  expect_equal(str_count(usage, "\n"), 5)
})

test_that("breaking works after escapes (#265)", {

  out <- roc_proc_text(rd_roclet(), "
    #' Long args
    f <- function(
      xxxxxxxxxxxxxxxxxx1,
      xxxxxxxxxxxxxxxxxx2,
      xxxxxxxxxxxxxxxxxx3,
      x = \"\\\"'\",
      xxxxxxxxxxxxxxxxxx4,
      xxxxxxxxxxxxxxxxxx5,
      xxxxxxxxxxxxxxxxxx6,
      xxxxxxxxxxxxxxxxxx7
  ){}")[[1]]
  usage <- format(get_tag(out, "usage"))
  expect_equal(str_count(usage, "\n"), 4)
})

test_that("replacement funs get non-breaking spaces", {
  out <- roc_proc_text(rd_roclet(), "
    #' Function long usage
    `long_replacement_fun<-` <- function(x,
        a = 'aaaaaaaaaaaaaaaa',
        b = 'aaaaaaaaaaaaaaaa',
        value) {}
  ")[[1]]
  usage <- format(get_tag(out, "usage"))
  expect_match(usage, " <- ")
})
