test_that("@usage overrides default", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A
    #' @usage a(a=2)
    a <- function(a=1) {}"
  )[[1]]
  expect_equal(out$get_value("usage"), rd("a(a=2)"))
})

test_that("@usage overrides default for @docType data", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title.
    #'
    #' @name abc
    #' @docType data
    #' @usage data(abc)
    NULL"
  )[[1]]

  expect_equal(out$get_value("usage"), rd("data(abc)"))
})

test_that("@usage NULL suppresses default usage", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' A
    #' @usage NULL
    a <- function(a=1) {}"
  )[[1]]

  expect_equal(out$get_value("usage"), NULL)
})

test_that("quoted topics have usage statements", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title.
    \"f\" <- function(a = 1, b = 2, c = a + b) {}"
  )[[1]]

  expect_equal(out$get_value("usage"), rd("f(a = 1, b = 2, c = a + b)"))
  expect_equal(
    out$get_rd("usage"),
    r"(\usage{
f(a = 1, b = 2, c = a + b)
})"
  )
})

# Escaping --------------------------------------------------------------------

test_that("usage escaping preserved when combined", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Foo
    foo <- function(x = '%') x

    #' @rdname foo
    bar <- function(y = '%') y
  "
  )[[1]]

  expect_s3_class(out$get_value("usage"), "rd")
})

test_that("default usage not double escaped", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Regular
    mean.foo <- function(x) 'foo'
  "
  )[[1]]

  expect_equal(
    out$get_rd("usage"),
    r"(\usage{
\method{mean}{foo}(x)
})"
  )
})

test_that("% and \\ are escaped in usage", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title.
    a <- function(a='%\\\\') {}"
  )[[1]]
  expect_equal(out$get_value("usage"), escape('a(a = "%\\\\")'))
  expect_equal(
    out$get_rd("usage"),
    r"[\usage{
a(a = "\%\\\\")
}]"
  )
})

test_that("% and \\ not escaped in manual usage", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title.
    #' @usage %\\
    a <- function(a) {}
  "
  )[[1]]
  expect_equal(out$get_value("usage"), rd('%\\'))
  expect_equal(
    out$get_rd("usage"),
    r"(\usage{
%\
})"
  )
})

test_that("Special vars removed in rc methods usage", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Class Blob
    ABCD <- setRefClass('ABC', methods = list(
      draw = function(x = 1) {
        \"2\"
        x
      })
    )
  "
  )[[1]]

  expect_equal(out$get_value("rcmethods"), list("draw(x = 1)" = "2"))
})

# object_usage ------------------------------------------------------------

test_that("usage captured from formals", {
  expect_equal(
    call_to_usage(f <- function() {}),
    "f()"
  )
  expect_equal(
    call_to_usage(f <- function(a = 1) {}),
    "f(a = 1)"
  )
})

test_that("argument containing function is generates correct usage", {
  expect_equal(
    call_to_usage(f <- function(a = function(x) 1) {}),
    "f(a = function(x) 1)"
  )
})

test_that("backticks retained when needed", {
  expect_equal(
    call_to_usage(f <- function(`_a`) {}),
    "f(`_a`)"
  )

  expect_equal(
    call_to_usage(`-f` <- function(x) {}),
    "`-f`(x)"
  )
})

test_that("% escaped when not in infix function", {
  expect_equal(
    call_to_usage(`%foo%bar` <- function(x, table) {}),
    r"(`\%foo\%bar`(x, table))"
  )
  expect_equal(
    call_to_usage(`%foo%bar<-` <- function(x, value) {}),
    r"(`\%foo\%bar`(x) <- value)"
  )
})

test_that("default usage formats data correctly", {
  expect_equal(
    call_to_usage(hello <- 1),
    "hello"
  )
})

test_that("default usage formats replacement functions correctly", {
  expect_equal(
    call_to_usage(`f<-` <- function(x, value) {}),
    "f(x) <- value"
  )
  expect_equal(
    call_to_usage(`f<-` <- function(x, y, value) {}),
    "f(x, y) <- value"
  )
})

test_that("default usage formats infix functions correctly", {
  expect_equal(call_to_usage("%.%" <- function(a, b) {}), r"(a \%.\% b)")
  expect_equal(call_to_usage(":" <- function(a, b) {}), "a:b")
  expect_equal(call_to_usage("+" <- function(a, b) {}), "a + b")

  # even if it contains <-
  expect_equal(call_to_usage("%<-%" <- function(a, b) {}), r"(a \%<-\% b)")

  # defaults are ignored
  expect_equal(call_to_usage(":" <- function(a = 1, b = 2) {}), "a:b")
})

test_that("default usage formats S3 methods correctly", {
  expect_equal(
    call_to_usage(mean.foo <- function(x) {}),
    r"(\method{mean}{foo}(x))"
  )
  expect_equal(
    call_to_usage(mean.function <- function(x) {}),
    r"(\method{mean}{`function`}(x))"
  )
  expect_equal(
    call_to_usage("+.foo" <- function(x, b) {}),
    r"(\method{+}{foo}(x, b))"
  )
  expect_equal(
    call_to_usage("%%.foo" <- function(x, b) {}),
    r"(\method{\%\%}{foo}(x, b))"
  )
  expect_equal(
    call_to_usage("[<-.foo" <- function(x, value) {}),
    r"(\method{[}{foo}(x) <- value)"
  )
})

test_that("S4 classes have no default usage", {
  expect_equal(
    call_to_usage({
      setClass("Foo")
    }),
    character()
  )
})

test_that("default usage correct for S4 generics", {
  expect_equal(
    call_to_usage({
      setGeneric("foo", function(x, y) {})
    }),
    "foo(x, y)"
  )
})

test_that("default usage correct for S4 methods", {
  expect_equal(
    call_to_usage({
      setClass("Foo")
      setMethod("sum", "Foo", function(x, ..., na.rm = FALSE) {})
    }),
    r"(\S4method{sum}{Foo}(x, ..., na.rm = FALSE))"
  )

  expect_equal(
    call_to_usage({
      setClass("Foo")
      setMethod("+", "Foo", function(e1, e2) "foo")
    }),
    r"(\S4method{+}{Foo,ANY}(e1, e2))"
  )

  expect_equal(
    call_to_usage({
      setClass("Foo")
      setMethod("[<-", "Foo", function(x, i, j, ..., value) "foo")
    }),
    r"(\S4method{[}{Foo}(x, i, j, ...) <- value)"
  )

  expect_equal(
    call_to_usage({
      setGeneric("%&&%", function(x, y) standardGeneric("%&&%"))
      setMethod("%&&%", signature("logical", "logical"), function(x, y) {})
    }),
    r"(\S4method{\%&&\%}{logical,logical}(x, y))"
  )
})

test_that("default usage correct for S4 methods with different args to generic", {
  expect_equal(
    call_to_usage({
      setGeneric("testfun", function(x, ...) standardGeneric("testfun"))
      setMethod("testfun", "matrix", function(x, add = FALSE, ...) {
        x - 1
      })
    }),
    r"(\S4method{testfun}{matrix}(x, add = FALSE, ...))"
  )
})

test_that("non-syntactic S4 class names are not escaped in usage", {
  expect_equal(
    call_to_usage({
      setGeneric("rhs", function(x) standardGeneric("rhs"))
      setMethod("rhs", "<-", function(x) x[[3]])
    }),
    r"(\S4method{rhs}{<-}(x))"
  )
})


# Wrapping --------------------------------------------------------------------

test_that("new wrapping style doesn't change unexpectedly", {
  expect_snapshot_output({
    cat(
      call_to_usage({
        f <- function(
          a = '                                    a',
          b = '                                    b',
          c = '                                    c',
          d = '                                    d'
        ) {}
      }),
      "\n\n"
    )

    cat(
      call_to_usage({
        f <- function(
          a = c(
            'abcdef',
            'abcdef',
            'abcdef',
            'abcdef',
            'abcdef',
            'abcdef',
            'abcdef',
            'abcdef',
            'abcdef',
            'abcdef'
          )
        ) {}
      }),
      "\n\n"
    )

    cat(
      call_to_usage({
        mean.reallyratherquitelongclassname <-
          function(
            reallyreatherquitelongargument = 'reallyratherquitelongvalue_____________________'
          ) {}
      }),
      "\n\n"
    )

    cat(
      call_to_usage({
        `long_replacement_fun<-` <- function(
          x,
          a = 'aaaaaaaaaaaaaaaa',
          b = 'aaaaaaaaaaaaaaaa',
          c = 'aaaaaaaaaaaaaaaa',
          value
        ) {}
      }),
      "\n\n"
    )

    cat(
      call_to_usage({
        function_name <- function(
          x,
          y,
          xy = "abcdef",
          xyz = c(
            `word word word word` = "abcdef",
            `word word word` = "abcdef",
            `word word word` = "abcdef",
            `word word word` = "abcdef"
          )
        ) {}
      }),
      "\n\n"
    )

    cat(
      call_to_usage({
        function_name <- function(
          f = function(x) {
            1
            2
          }
        ) {}
      }),
      "\n\n"
    )
  })
})

test_that("old wrapping style doesn't change unexpectedly", {
  local_roxy_meta_set("old_usage", TRUE)

  expect_snapshot_output({
    cat(
      call_to_usage({
        f <- function(
          a = '                                    a',
          b = '                                    b',
          c = '                                    c',
          d = '                                    d'
        ) {}
      }),
      "\n\n"
    )

    cat(
      call_to_usage({
        f <- function(
          a = c(
            'abcdef',
            'abcdef',
            'abcdef',
            'abcdef',
            'abcdef',
            'abcdef',
            'abcdef',
            'abcdef',
            'abcdef',
            'abcdef'
          )
        ) {}
      }),
      "\n\n"
    )

    cat(
      call_to_usage({
        mean.reallyratherquitelongclassname <-
          function(
            reallyreatherquitelongargument = 'reallyratherquitelongvalue_____________________'
          ) {}
      }),
      "\n\n"
    )

    cat(
      call_to_usage({
        `long_replacement_fun<-` <- function(
          x,
          a = 'aaaaaaaaaaaaaaaa',
          b = 'aaaaaaaaaaaaaaaa',
          c = 'aaaaaaaaaaaaaaaa',
          value
        ) {}
      }),
      "\n\n"
    )

    # breaking works after escapes (#265)
    cat(
      call_to_usage({
        f <- function(
          xxxxxxxxxxxxxxxxxx1,
          xxxxxxxxxxxxxxxxxx2,
          xxxxxxxxxxxxxxxxxx3,
          x = "\"'",
          xxxxxxxxxxxxxxxxxx4,
          xxxxxxxxxxxxxxxxxx5,
          xxxxxxxxxxxxxxxxxx6,
          xxxxxxxxxxxxxxxxxx7
        ) {}
      }),
      "\n\n"
    )
  })
})

test_that("preserves non-breaking-space", {
  expect_equal(
    call_to_usage(f <- function(a = "\u{A0}") {}),
    'f(a = "\u{A0}")'
  )
})
