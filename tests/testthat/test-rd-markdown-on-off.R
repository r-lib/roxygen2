context("Rd: turning markdown on/off")
roc <- rd_roclet()

test_that("turning on/off markdown globally", {
  ## off
  out1 <- roc_proc_text(roc, global_options = list(markdown = FALSE), "
    #' Title
    #'
    #' Description with some `code` included. `More code.`
    foo <- function() {}")[[1]]
  expect_equal(
    get_tag(out1, "description")$values,
    "Description with some `code` included. `More code.`"
  )

  ## on
  out1 <- roc_proc_text(roc, global_options = list(markdown = TRUE), "
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
  out1 <- roc_proc_text(roc, global_options = list(markdown = FALSE), "
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
  out1 <- roc_proc_text(roc, global_options = list(markdown = FALSE), "
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
  out1 <- roc_proc_text(roc, global_options = list(markdown = TRUE), "
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
  out1 <- roc_proc_text(roc, global_options = list(markdown = TRUE), "
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
    out1 <- roc_proc_text(roc, "
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
    out1 <- roc_proc_text(roc, global_options = list(markdown = FALSE), "
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
    out1 <- roc_proc_text(roc, global_options = list(markdown = TRUE), "
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
