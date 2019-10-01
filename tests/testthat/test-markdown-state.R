test_that("markdown is off by default", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description with some `code` included. `More code.`
    foo <- function() {}")[[1]]
  expect_equal(
    out1$get_value("description"),
    "Description with some `code` included. `More code.`"
  )
})

test_that("turning on/off markdown globally", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description with some `code` included. `More code.`
    foo <- function() {}")[[1]]
  expect_equal(
    out1$get_value("description"),
    "Description with some `code` included. `More code.`"
  )

  old <- roxy_meta_set("markdown", TRUE)
  on.exit(roxy_meta_set("markdown", old))
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description with some `code` included. `More code.`
    foo <- function() {}")[[1]]
  expect_equal(
    out1$get_value("description"),
    "Description with some \\code{code} included. \\verb{More code.}"
  )
})

test_that("turning on/off markdown locally", {
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description with some `code` included. `More code.`
    #' @noMd
    foo <- function() {}")[[1]]
  expect_equal(
    out1$get_value("description"),
    "Description with some `code` included. `More code.`"
  )

  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description with some `code` included. `More code.`
    #' @md
    foo <- function() {}")[[1]]
  expect_equal(
    out1$get_value("description"),
    "Description with some \\code{code} included. \\verb{More code.}"
  )

  old <- roxy_meta_set("markdown", TRUE)
  on.exit(roxy_meta_set("markdown", old))
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description with some `code` included. `More code.`
    #' @noMd
    foo <- function() {}")[[1]]
  expect_equal(
    out1$get_value("description"),
    "Description with some `code` included. `More code.`"
  )

  ## on / on
  out1 <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' Description with some `code` included. `More code.`
    #' @md
    foo <- function() {}")[[1]]
  expect_equal(
    out1$get_value("description"),
    "Description with some \\code{code} included. \\verb{More code.}"
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
    out1$get_value("description"),
    "Description with some `code` included. `More code.`"
  )

  old <- roxy_meta_set("markdown", TRUE)
  on.exit(roxy_meta_set("markdown", old))
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
    out1$get_value("description"),
    "Description with some `code` included. `More code.`"
  )
})
