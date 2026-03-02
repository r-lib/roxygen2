test_that("invalid syntax generates useful warning", {
  block <- "
    #' A
    #' @field
    #' @slot
    a <- function() {}
  "

  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))
})

test_that("@fields creates a new section and lists fields", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Important class.
    #'
    #' @field a field a
    #'
    #' @field b field b
    #'
    setRefClass('test')
    "
  )[[1]]
  expect_equal(out$get_value("field"), c(a = "field a", b = "field b"))
})

test_that("@slot creates a new section and lists slots", {
  out <- roc_proc_text(
    rd_roclet(),
    "
      #' Important class.
      #'
      #' @slot a slot a
      #' @slot b slot b
      setClass('test')
    "
  )[[1]]
  expect_equal(out$get_value("slot"), c(a = "slot a", b = "slot b"))
})
