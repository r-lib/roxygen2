context("Rd: inherit")

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
  parsed <- parse_text("
    #' @inherit fun
    NULL
  ")
  block <- parsed$blocks[[1]]

  expect_equal(block$inherit$inherit, c("params", "slots", "return"))
})
