test_that("r6_extract_field_tags builds rd_r6_field objects", {
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
  expect_silent(fields <- r6_extract_field_tags(block, r6data, "field"))
  expect_equal(
    fields$fields,
    list(
      rd_r6_field("field1", "Foo."),
      rd_r6_field("field2", "Bar.")
    )
  )
})

test_that("comma-separated @field works (#1600)", {
  text <- "
    #' Class
    C <- R6::R6Class(
      public = list(
        #' @field var_1,var_2 do something vars
        var_1 = 1,
        var_2 = 2
      )
    )"
  expect_silent(docs <- r6_doc(text))
  expect_equal(
    docs$fields$fields,
    list(rd_r6_field("var_1, var_2", "do something vars"))
  )
})

test_that("r6_extract_field_tags builds active binding objects", {
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
  expect_silent(bindings <- r6_extract_field_tags(block, r6data, "active"))
  expect_equal(
    bindings$fields,
    list(
      rd_r6_field("bind1", "Active binding."),
      rd_r6_field("bind2", "Active 2.")
    )
  )
})

test_that("@field name NULL suppresses field documentation", {
  text <- "
    #' Class
    #' @field field1 Foo.
    #' @field field2 NULL
    C <- R6::R6Class(
      public = list(
        field1 = NULL,
        field2 = 'bar'
      )
    )"
  block <- parse_text(text)[[1]]
  r6data <- block_get_tag_value(block, ".r6data")
  expect_silent(fields <- r6_extract_field_tags(block, r6data, "field"))
  expect_equal(
    fields$fields,
    list(rd_r6_field("field1", "Foo."))
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
  expect_equal(docs$fields$fields, list())
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

test_that("format.rd_r6_field produces \\item markup", {
  expect_snapshot(cat(format(rd_r6_field("x", "A number."))))
})

test_that("format.rd_r6_fields produces fields & bindings sections", {
  fields <- rd_r6_fields(list(
    rd_r6_field("x", "A number."),
    rd_r6_field("y", "A string.")
  ))
  expect_snapshot(cat(format(fields), sep = "\n"))

  bindings <- rd_r6_fields(
    list(rd_r6_field("val", "A value.")),
    type = "active"
  )
  expect_snapshot(cat(format(bindings), sep = "\n"))
})

test_that("format.rd_r6_fields returns nothing when empty", {
  expect_null(format(rd_r6_fields()))
})

# Superclass field inheritance -----------------------------------------------

test_that("fields inherit docs from superclass", {
  text <- "
    #' Parent
    A <- R6::R6Class('A', cloneable = FALSE,
      public = list(
        #' @field x A field.
        x = NULL
      )
    )

    #' Child
    B <- R6::R6Class('B', cloneable = FALSE,
      inherit = A,
      public = list(
        x = NULL
      )
    )"
  expect_silent(docs <- r6_doc(text))
  expect_equal(
    docs$fields$fields,
    list(rd_r6_field("x", "A field."))
  )
})

test_that("field docs in subclass override superclass", {
  text <- "
    #' Parent
    A <- R6::R6Class('A', cloneable = FALSE,
      public = list(
        #' @field x Parent field.
        x = NULL
      )
    )

    #' Child
    #' @field x Child field.
    B <- R6::R6Class('B', cloneable = FALSE,
      inherit = A,
      public = list(
        x = NULL
      )
    )"
  docs <- r6_doc(text)
  expect_equal(
    docs$fields$fields,
    list(rd_r6_field("x", "Child field."))
  )
})
