context("Slot")
roc <- rd_roclet()

test_that("@slot creates a new section and lists slots", {
  out <- roc_proc_text(roc, "
      #' Important class.
      #' 
      #' @slot a slot a
      #' @slot b slot b
      setClass('test')
    ")[[1]]
  expect_equal(get_tag(out, "slot")$values, c(a = "slot a", b = "slot b"))
})
