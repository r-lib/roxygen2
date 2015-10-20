context("Keyword")

test_that("keywords split into pieces", {
  out <- roc_proc_text(rd_roclet(), "
    #' @keywords a b
    #' @name a
    NULL")[[1]]

  expect_match(get_tag(out, "keyword")$values, fixed("a"), all = FALSE)
  expect_match(get_tag(out, "keyword")$values, fixed("b"), all = FALSE)
})
