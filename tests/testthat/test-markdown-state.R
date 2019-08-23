test_that("markdown is off by default", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description with some `code` included. `More code.`
    foo <- function() {}")[[1]]
  expect_equal(
    get_tag(out1, "description")$values,
    "Description with some `code` included. `More code.`"
  )
})

test_that("turning on/off markdown globally", {
  ## off
  out1 <- roc_proc_text(rd_roclet(), global_options = list(markdown = FALSE), "
    #' Title
    #'
    #' Description with some `code` included. `More code.`
    foo <- function() {}")[[1]]
  expect_equal(
    get_tag(out1, "description")$values,
    "Description with some `code` included. `More code.`"
  )

  ## on
  out1 <- roc_proc_text(rd_roclet(), global_options = list(markdown = TRUE), "
    #' Title
    #'
    #' Description with some `code` included. `More code.`
    foo <- function() {}")[[1]]
  expect_equal(
    get_tag(out1, "description")$values,
    "Description with some \\code{code} included. \\code{More code.}"
  )
})

test_that("turning on/off markdown locally", {
  ## off / off
  out1 <- roc_proc_text(rd_roclet(), global_options = list(markdown = FALSE), "
    #' Title
    #'
    #' Description with some `code` included. `More code.`
    #' @noMd
    foo <- function() {}")[[1]]
  expect_equal(
    get_tag(out1, "description")$values,
    "Description with some `code` included. `More code.`"
  )

  ## off / on
  out1 <- roc_proc_text(rd_roclet(), global_options = list(markdown = FALSE), "
    #' Title
    #'
    #' Description with some `code` included. `More code.`
    #' @md
    foo <- function() {}")[[1]]
  expect_equal(
    get_tag(out1, "description")$values,
    "Description with some \\code{code} included. \\code{More code.}"
  )

  ## on / off
  out1 <- roc_proc_text(rd_roclet(), global_options = list(markdown = TRUE), "
    #' Title
    #'
    #' Description with some `code` included. `More code.`
    #' @noMd
    foo <- function() {}")[[1]]
  expect_equal(
    get_tag(out1, "description")$values,
    "Description with some `code` included. `More code.`"
  )

  ## on / on
  out1 <- roc_proc_text(rd_roclet(), global_options = list(markdown = TRUE), "
    #' Title
    #'
    #' Description with some `code` included. `More code.`
    #' @md
    foo <- function() {}")[[1]]
  expect_equal(
    get_tag(out1, "description")$values,
    "Description with some \\code{code} included. \\code{More code.}"
  )

})

test_that("warning for both @md and @noMd", {
  expect_warning(
    out1 <- roc_proc_text(rd_roclet(), "
      #' Title
      #'
      #' Description with some `code` included. `More code.`
      #' @md
      #' @noMd
      foo <- function() {}")[[1]],
    "Both @md and @noMd, no markdown parsing"
  )
  expect_equal(
    get_tag(out1, "description")$values,
    "Description with some `code` included. `More code.`"
  )

  expect_warning(
    out1 <- roc_proc_text(rd_roclet(), global_options = list(markdown = FALSE), "
      #' Title
      #'
      #' Description with some `code` included. `More code.`
      #' @md
      #' @noMd
      foo <- function() {}")[[1]],
    "Both @md and @noMd, no markdown parsing"
  )
  expect_equal(
    get_tag(out1, "description")$values,
    "Description with some `code` included. `More code.`"
  )

  expect_warning(
    out1 <- roc_proc_text(rd_roclet(), global_options = list(markdown = TRUE), "
      #' Title
      #'
      #' Description with some `code` included. `More code.`
      #' @md
      #' @noMd
      foo <- function() {}")[[1]],
    "Both @md and @noMd, no markdown parsing"
  )
  expect_equal(
    get_tag(out1, "description")$values,
    "Description with some `code` included. `More code.`"
  )
})
