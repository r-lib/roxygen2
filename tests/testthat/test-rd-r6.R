
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

test_that("r6_fields", {
  text <- "
    #' @title Title
    #' @description Description.
    #' @details Details.
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
  doc <- r6_fields(block, r6data)

  expect_true(any(grepl("field1\\tab Foo.", doc, fixed = TRUE)))
  expect_true(any(grepl("field2\\tab Bar.", doc, fixed = TRUE)))
})
