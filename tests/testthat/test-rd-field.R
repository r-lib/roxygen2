context("Rd: field")

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
  expect_equal(out$get_value("field"), c(a = "field a", b = "field b"))
})
