context("C++ Preparser")

dropAttrs <- function(x) {
  rapply(x, function(xx) {
    attributes(xx) <- NULL
    xx
  }, how = "list")
}

test_that("The C++ parser works on example blocks / files", {

  block <- readLines("roxygen-block-1.R")
  parsed <- preparse_block(paste(block, collapse = "\n"))
  block_1_expectation <- list(
    introduction = "Title\n\nDescription\n\nDetails\n\n",
    param = "x,y,z Descriptions for x, y, z",
    param = "a Description of a",
    param = "b\nDescription of b",
    section = "Important:\nDon't run with scissors!",
    export = ""
  )

  expect_identical(
    dropAttrs(parsed),
    block_1_expectation
  )

  expect_equal(
    unlist(lapply(parsed, function(x) attr(x, "row")), use.names = FALSE),
    c(1, 7, 8, 9, 11, 13)
  )

  parsed <- preparse_file("roxygen-example-1.R")
  expect_identical(
    dropAttrs(parsed),
    list(
      block_1_expectation,
      block_1_expectation
    )
  )

})
