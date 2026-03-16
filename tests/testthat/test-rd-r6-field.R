test_that("r6_extract_fields builds rd_r6_field objects", {
  text <- "
    #' @field field1 Foo.
    #' @field field2 Bar.
    C <- R6::R6Class(
      public = list(
        field1 = NULL,
        field2 = \"foobar\"
      )
    )"
  block <- parse_text(text)[[1]]
  r6data <- block_get_tag_value(block, ".r6data")
  expect_silent(fields <- r6_extract_fields(block, r6data))
  expect_equal(
    fields,
    list(
      rd_r6_field("field1", "Foo."),
      rd_r6_field("field2", "Bar.")
    )
  )
})

test_that("r6_extract_active_bindings builds rd_r6_field objects", {
  text <- "
    #' @field bind1 Active binding.
    #' @field bind2 Active 2.
    C <- R6::R6Class(
      active = list(
        bind1 = function(x) { },
        bind2 = function(x) { }
      )
    )"
  block <- parse_text(text)[[1]]
  r6data <- block_get_tag_value(block, ".r6data")
  expect_silent(bindings <- r6_extract_active_bindings(block, r6data))
  expect_equal(
    bindings,
    list(
      rd_r6_field("bind1", "Active binding."),
      rd_r6_field("bind2", "Active 2.")
    )
  )
})

test_that("warns about undocumented fields", {
  text <- "
    #' Class
    C <- R6::R6Class(
      public = list(
        undocumented_field = NULL
      )
    )"
  expect_snapshot(docs <- r6_doc(text))
  expect_length(docs$fields, 0)
})

test_that("warns about fields documented multiple times", {
  text <- "
    #' Class
    #' @field x First.
    #' @field x Second.
    C <- R6::R6Class(
      public = list(x = NULL)
    )"
  expect_snapshot(docs <- r6_doc(text))
})

test_that("warns about unknown fields", {
  text <- "
    #' Class
    #' @field nosuch Doesn't exist.
    C <- R6::R6Class(
      public = list(x = NULL)
    )"
  expect_snapshot(docs <- r6_doc(text))
})

test_that("warns about undocumented active bindings", {
  text <- "
    #' Class
    C <- R6::R6Class(
      active = list(
        undocumented = function(x) {}
      )
    )"
  expect_snapshot(docs <- r6_doc(text))
  expect_length(docs$active_bindings, 0)
})

test_that("warns about active bindings documented multiple times", {
  text <- "
    #' Class
    #' @field b First.
    #' @field b Second.
    C <- R6::R6Class(
      active = list(
        b = function(x) {}
      )
    )"
  expect_snapshot(docs <- r6_doc(text))
})

test_that("format.rd_r6_field produces \\item markup", {
  expect_snapshot(cat(format(rd_r6_field("x", "A number."))))
})
