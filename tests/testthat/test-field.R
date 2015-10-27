context("Field")

test_that("@fields creates a new section and lists fields", {
  out <- roc_proc_text(rd_roclet(), "
    #' Important class.
    #'
    #' @field a field a
    #'
    #' @field b field b
    #'
    setRefClass('test')
    ")[[1]]
  expect_equal(get_tag(out, "field")$values, c(a = "field a", b = "field b"))
})
