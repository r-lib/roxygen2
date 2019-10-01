
test_that("extract_r6_fields", {
  C <- R6::R6Class(
    public = list(
      field1 = NULL,
      meth1 = function() { },
      field2 = "foobar"
    )
  )
  F <- extract_r6_fields(C)
  expect_equal(F$type, rep("field", 2))
  expect_equal(F$name, c("field1", "field2"))

  C <- R6::R6Class(
    public = list(
      meth1 = function() { }
    )
  )
  F <- extract_r6_fields(C)
  expect_s3_class(F, "data.frame")
  expect_equal(F$type, character())
  expect_equal(F$name, character())

  C <- R6::R6Class()
  F <- extract_r6_fields(C)
  expect_s3_class(F, "data.frame")
  expect_equal(F$type, character())
  expect_equal(F$name, character())
})

test_that("extract_r6_bindings", {
  C <- R6::R6Class(
    active = list(
      bind1 = function(x) { },
      bind2 = function(x) { }
    ),
    public = list(
      meth1 = function() { }
    )
  )
  F <- extract_r6_bindings(C)
  expect_equal(F$type, rep("active", 2))
  expect_equal(F$name, c("bind1", "bind2"))

  C <- R6::R6Class(
    public = list(
      meth1 = function() { }
    )
  )
  F <- extract_r6_bindings(C)
  expect_s3_class(F, "data.frame")
  expect_equal(F$type, character())
  expect_equal(F$name, character())

  C <- R6::R6Class()
  F <- extract_r6_bindings(C)
  expect_s3_class(F, "data.frame")
  expect_equal(F$type, character())
  expect_equal(F$name, character())
})

test_that("r6_fields", {
  text <- "
    #' @title Title
    #' @description Description.
    #' @details Details.
    #' @field field1 Foo.
    #' @field field2 Bar.
    #' @field bind1 Active binding.
    C <- R6::R6Class(
      public = list(
        field1 = NULL,
        field2 = \"foobar\"
      ),
      active = list(
        bind1 = function(x) { }
      )
    )"
  block <- parse_text(text)[[1]]
  r6data <- block_get_tag_value(block, ".r6data")
  expect_silent(doc <- r6_fields(block, r6data))

  expect_true(any(grepl("field1\\tab Foo.", doc, fixed = TRUE)))
  expect_true(any(grepl("field2\\tab Bar.", doc, fixed = TRUE)))
})

test_that("r6_active_bindings", {
  text <- "
    #' @title Title
    #' @description Description.
    #' @details Details.
    #' @field bind1 Active binding.
    #' @field bind2 Active 2.
    C <- R6::R6Class(
      public = list(
        field1 = NULL,
        field2 = \"foobar\"
      ),
      active = list(
        bind1 = function(x) { },
        bind2 = function(x) { }
      )
    )"
  block <- parse_text(text)[[1]]
  r6data <- block_get_tag_value(block, ".r6data")
  expect_silent(doc <- r6_active_bindings(block, r6data))

  expect_true(any(grepl("bind1\\tab Active binding.", doc, fixed = TRUE)))
  expect_true(any(grepl("bind2\\tab Active 2.", doc, fixed = TRUE)))
})
