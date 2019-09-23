test_that("@slot creates a new section and lists slots", {
  out <- roc_proc_text(rd_roclet(), "
      #' Important class.
      #'
      #' @slot a slot a
      #' @slot b slot b
      setClass('test')
    ")[[1]]
  expect_equal(out$get_value("slot"), c(a = "slot a", b = "slot b"))
})
