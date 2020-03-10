test_that("end-to-end NAMESPACE generation works", {
  expect_output(roxygenise(test_path("testNamespace"), "namespace", clean = TRUE))

  ns <- read_lines(test_path("testNamespace/NAMESPACE"))
  expect_length(ns, 4)
})

# @export -----------------------------------------------------------------

test_that("export quote object name appropriate", {
  out <- roc_proc_text(namespace_roclet(), "#' @export\na <- function(){}")
  expect_equal(out, 'export(a)')

  out <- roc_proc_text(namespace_roclet(), "#' @export\n`+` <- function(){}")
  expect_equal(out, 'export("+")')

  out <- roc_proc_text(namespace_roclet(), "#' @export\n`\\`` <- function(){}")
  expect_equal(out, 'export("`")')
})

test_that("export parameter overrides default", {
  out <- roc_proc_text(namespace_roclet(), "#' @export b\na <- function(){}")
  expect_equal(out, 'export(b)')
})

test_that("multiple export parameters generate multiple exports", {
  out <- roc_proc_text(namespace_roclet(), "
    #' @export a b
    a <- function(){}")
  expect_equal(out, c('export(a)', 'export(b)'))
})

test_that("export trimmed before line test", {
  out <- roc_proc_text(namespace_roclet(), "
    #' @export
    #'
    a <- function(){}")
  expect_equal(out, 'export(a)')
})


test_that("export detects S4 class", {
  out <- roc_proc_text(namespace_roclet(), "#' @export\nsetClass('a')")
  expect_equal(out, 'exportClasses(a)')
})

test_that("export detects S4 generic", {
  out <- roc_proc_text(namespace_roclet(), "
    #' @export
    setGeneric('foo', function(x) standardGeneric('foo'))
  ")
  expect_equal(out, 'export(foo)')
})


test_that("export detects S3 method", {
  out <- roc_proc_text(namespace_roclet(), "#' @export\nmean.foo <- function(x) 'foo'")
  expect_equal(out, 'S3method(mean,foo)')
})

test_that("export handles non-syntactic names", {
  out <- roc_proc_text(namespace_roclet(),  "
    #' @export
    `mean.foo-bar` <- function(x) 'foo'
  ")
  expect_equal(out, "S3method(mean,\"foo-bar\")")

  out <- roc_proc_text(namespace_roclet(),  "
    `foo-bar` <- function(x) UseMethod('foo-bar')
    #' @export
    `foo-bar.integer` <- function(x) 'foo'
  ")
  expect_equal(out, "S3method(\"foo-bar\",integer)")
})

test_that("@exportS3method generatedsS3method()", {
  out <- roc_proc_text(namespace_roclet(),
    "#' @exportS3Method
    mean.foo <- function(x) 'foo'
  ")
  expect_equal(out, "S3method(mean,foo)")

  out <- roc_proc_text(namespace_roclet(),
    "#' @exportS3Method base::mean
    mean.foo <- function(x) 'foo'
  ")
  expect_equal(out, "S3method(base::mean,foo)")

  expect_warning(
    roc_proc_text(namespace_roclet(),
      "#' @exportS3Method base::mean
      NULL
    "),
    "an S3 method"
  )

  out <- roc_proc_text(namespace_roclet(),
    "#' @exportS3Method base::mean foo
    NULL
  ")
  expect_equal(out, "S3method(base::mean,foo)")


})

test_that("exportClass overrides default class name", {
  out <- roc_proc_text(namespace_roclet(), "#' @exportClass b\nsetClass('a')")
  expect_equal(out, 'exportClasses(b)')
})

test_that("export detects method name", {
  out <- roc_proc_text(namespace_roclet(), "
    #' @export\n
    setMethod('max', 'a', function(x, ...) x[1])")
  expect_equal(out, 'exportMethods(max)')
})

test_that("export method escapes if needed", {
  out <- roc_proc_text(namespace_roclet(), "
    setGeneric('x<-', function(x, value) standardGeneric('x<-'))
    #' @export\n
    setMethod('x<-', 'a', function(x, value) value)")
  expect_equal(out, 'exportMethods("x<-")')
})

test_that("export uses name if no object present", {
  out <- roc_proc_text(namespace_roclet(), "
    #' Title
    #'
    #' @export
    #' @name x
    NULL
  ")
  expect_equal(out, 'export(x)')
})

test_that("default export uses exportClass for RC objects", {
  out <- roc_proc_text(namespace_roclet(), "
    #' Title
    #'
    #' @export
    x <- setRefClass('X')
  ")
  expect_equal(out, 'exportClasses(X)')
})

test_that("exportMethod overrides default method name", {
  out <- roc_proc_text(namespace_roclet(), "
    #' @exportMethod c
    setMethod('max', 'a', function(x, ...) x[1])")
  expect_equal(out, 'exportMethods(c)')
})

test_that("other namespace tags produce correct output", {
  out <- roc_proc_text(namespace_roclet(), "
    #' @exportPattern test
    #' @import test
    #' @importFrom test test1 test2
    #' @importClassesFrom test test1 test2
    #' @importMethodsFrom test test1 test2
    NULL")

  expect_equal(sort(out), sort(c(
    "exportPattern(test)",
    "import(test)",
    "importFrom(test,test1)",
    "importFrom(test,test2)",
    "importClassesFrom(test,test1)",
    "importClassesFrom(test,test2)",
    "importMethodsFrom(test,test1)",
    "importMethodsFrom(test,test2)"
  )))
})

test_that("poorly formed importFrom throws error", {
  expect_warning(roc_proc_text(namespace_roclet(), "
    #' @importFrom test
    NULL
  "), "needs at least 2 words")
})

test_that("multiline importFrom parsed correctly", {
  out <- roc_proc_text(namespace_roclet(), "
    #' @importFrom test test1
    #'   test2
    NULL
  ")
  expect_equal(sort(out), sort(c(
    "importFrom(test,test1)",
    "importFrom(test,test2)"
  )))
})

test_that("useDynLib imports only selected functions", {
  out <- roc_proc_text(namespace_roclet(), "
    #' @useDynLib test
    #' @useDynLib test a
    #' @useDynLib test a b
    NULL")

    expect_equal(sort(out), sort(
      c("useDynLib(test)", "useDynLib(test,a)", "useDynLib(test,b)")))
})

test_that("useDynLib doesn't quote if comma present", {
  out <- roc_proc_text(namespace_roclet(), "
    #' @useDynLib test, .registration = TRUE
    NULL")

  expect_equal(sort(out), "useDynLib(test, .registration = TRUE)")
})

test_that("empty NAMESPACE generates zero-length vector", {
  base_path <- test_path("empty")

  env <- pkgload::load_all(base_path)$env
  blocks <- parse_package(base_path, env = env)

  results <- roclet_process(namespace_roclet(), blocks, env = env, base_path)
  expect_equal(results, character())
})


# Raw ---------------------------------------------------------------------


test_that("rawNamespace must be valid code", {
  expect_warning(
    roc_proc_text(namespace_roclet(), "
      #' @rawNamespace if() {
      #' @name a
      NULL"),
    "code failed to parse"
  )
})

test_that("rawNamespace inserted unchanged", {
  out <- roc_proc_text(namespace_roclet(), "
    #' @rawNamespace xyz
    #'   abc
    NULL")

  expect_equal(out, "xyz\n  abc")
})


# @evalNamespace ----------------------------------------------------------

test_that("evalNamespace generates warning when code is invalid", {
  expect_warning(
    roc_proc_text(namespace_roclet(), "
      #' @evalNamespace a +
      #' @name a
      #' @title a
      NULL"),
    "code failed to parse"  # Message generated by tag_code
  )
})

test_that("evalNamespace generates warning when code raises error", {
  expect_warning(
    roc_proc_text(namespace_roclet(), "
      #' @evalNamespace stop('!')
      #' @name a
      #' @title a
      NULL"),
    "failed with error"  # From block_eval
  )
})

test_that("evalNamespace generates warning when code doesn't eval to string", {
  # Not character
  expect_warning(
    roc_proc_text(namespace_roclet(), "
      z <- 10
      #' @evalNamespace z * 2
      #' @name a
      #' @title a
      NULL"),
    "did not evaluate to a string"  # From block_eval
  )

  # NA_character_ not allowed
  expect_warning(
    roc_proc_text(namespace_roclet(), "
      nms <- NA_character_
      #' @evalNamespace nms
      #' @name a
      #' @title a
      NULL"),
    "result contained NA"  # From block_eval
  )
})

test_that("evalNamespace code is inserted when its value is a string", {
  out1 <- roc_proc_text(namespace_roclet(), "
    nms <- paste(letters[1:3], collapse = ',')
    #' @evalNamespace sprintf('export(%s)', nms)
    #' @name a
    #' @title a
    NULL")
  out2 <- roc_proc_text(namespace_roclet(), "
    nms <- paste(letters[1:3], collapse = ',')
    #' @evalNamespace sprintf('export(%s)',
    #'                        nms)
    #' @name a
    #' @title a
    NULL")

  expect_equal(out1, "export(a,b,c)")
  expect_equal(out2, "export(a,b,c)")
})

test_that("evalNamspace can yield a vector", {
  out <- roc_proc_text(namespace_roclet(), "
    nms <- letters[1:2]
    #' @evalNamespace paste0('export(', nms, ')')
    #' @name a
    #' @title a
    NULL")

  expect_equal(out, c("export(a)", "export(b)"))
})


# helpers -----------------------------------------------------------------

test_that("auto_quote behaves as needed", {
  expect_equal(auto_quote("x"), "x")
  expect_equal(auto_quote("if"), '"if"') # quotes non-syntactic
  expect_equal(auto_quote("'if'"), "'if'") # unless already quoted
})
