# @R6method ----------------------------------------------------------------

test_that("@R6method tag parser validates input", {
  tag <- roxy_tag("R6method", "Foo$bar")
  result <- roxy_tag_parse(tag)
  expect_equal(result$val, list(class = "Foo", method = "bar"))

  expect_snapshot({
    . <- roxy_tag_parse(roxy_tag("R6method", ""))
    . <- roxy_tag_parse(roxy_tag("R6method", "nomethod"))
    . <- roxy_tag_parse(roxy_tag("R6method", "$bar"))
    . <- roxy_tag_parse(roxy_tag("R6method", "Foo$"))
    . <- roxy_tag_parse(roxy_tag("R6method", "Foo$bar\nextra"))
  })
})

test_that("@R6method merges external method into R6 class topic", {
  text <- "
    #' @title Title
    #' @description Description.
    C <- R6::R6Class('C', cloneable = FALSE,
      public = list(
        #' @description Method 1.
        meth1 = function() { }
      )
    )

    #' @R6method C$meth2
    #' @description Method 2.
    #' @param x A number.
    C$set('public', 'meth2', function(x) { })
  "

  env <- new.env(parent = globalenv())
  eval(parse(text = text, keep.source = TRUE), envir = env)
  blocks <- parse_text(text, env = env)
  roc <- roclet_preprocess(roclet_find("rd"))
  res <- roclet_process(roc, blocks = blocks, env = env, base_path = ".")
  rd <- format(res$C.Rd)

  expect_match(rd, "Method \\code{meth1()}", fixed = TRUE)
  expect_match(rd, "Method \\code{meth2()}", fixed = TRUE)
  expect_match(rd, "Method 2.", fixed = TRUE)
  expect_match(rd, "code{x}}{A number.", fixed = TRUE)
  expect_match(rd, "C$meth2(x)", fixed = TRUE)
})

test_that("@R6method works with indirect $set() calls", {
  text <- "
    #' @title Title
    #' @description Description.
    C <- R6::R6Class('C', cloneable = FALSE,
      public = list(
        #' @description Method 1.
        meth1 = function() { }
      )
    )

    name <- 'meth2'
    C$set('public', name, function(x) { })

    #' @R6method C$meth2
    #' @description Method 2.
    #' @param x A number.
    NULL
  "

  env <- new.env(parent = globalenv())
  eval(parse(text = text, keep.source = TRUE), envir = env)
  blocks <- parse_text(text, env = env)
  roc <- roclet_preprocess(roclet_find("rd"))
  res <- roclet_process(roc, blocks = blocks, env = env, base_path = ".")
  rd <- format(res$C.Rd)

  expect_match(rd, "C$meth2(x)", fixed = TRUE)
  expect_match(rd, "Method 2.", fixed = TRUE)
})

test_that("@R6method removes field from fields list", {
  text <- "
    #' @title Title
    #' @description Description.
    C <- R6::R6Class('C', cloneable = FALSE,
      public = list(
        bar = NULL,
        #' @description Method 1.
        meth1 = function() { }
      )
    )

    C$public_fields$bar <- NULL
    C$set('public', 'bar', function(x) { })

    #' @R6method C$bar
    #' @description Bar method.
    #' @param x A value.
    NULL
  "

  env <- new.env(parent = globalenv())
  eval(parse(text = text, keep.source = TRUE), envir = env)
  blocks <- parse_text(text, env = env)
  roc <- roclet_preprocess(roclet_find("rd"))
  res <- roclet_process(roc, blocks = blocks, env = env, base_path = ".")
  rd <- format(res$C.Rd)

  # bar should appear as a method, not a field
  expect_match(rd, "Method \\code{bar()}", fixed = TRUE)
  expect_no_match(rd, "Public fields", fixed = TRUE)
})

test_that("@R6method warns on unknown class", {
  text <- "
    #' @R6method NoSuchClass$meth
    #' @description A method.
    NULL
  "

  env <- new.env(parent = globalenv())
  eval(parse(text = text, keep.source = TRUE), envir = env)
  blocks <- parse_text(text, env = env)
  roc <- roclet_preprocess(roclet_find("rd"))
  expect_snapshot(
    roclet_process(roc, blocks = blocks, env = env, base_path = ".")
  )
})
