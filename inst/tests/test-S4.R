context("S4")
roc <- rd_roclet()

test_that ("@usage for S4methods",{
  out <- roc_proc_text(roc, "
    #' Title.
   setMethod (\"show\", signature = c (object = \"array\"),
      function (object) {})")[[1]]
    expect_equal(get_tag(out, "usage")$values,
               "\\S4method{show}{array}(object)")
})

test_that ("@alias for S4methods",{
  out <- roc_proc_text(roc, "
    #' Title.
   setMethod (\"show\", signature = c (object = \"array\"),
      function (object) {})")[[1]]
  expect_equal(get_tag(out, "alias")$values,
               c("show", "show,array-method"))
})
