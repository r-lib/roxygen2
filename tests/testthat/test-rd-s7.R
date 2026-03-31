test_that("@prop creates Additional properties section", {
  out <- roc_proc_text(
    rd_roclet(),
    "
      #' Important class.
      #'
      #' @prop a prop a
      #' @prop b prop b
      a <- function() {}
    "
  )[[1]]
  expect_equal(out$get_value("prop"), c(a = "prop a", b = "prop b"))
})
