context("Rd - docTypes")
roc <- rd_roclet()

# Package --------------------------------------------------------------------

test_that("@docType package automatically adds package alias when needed", {
  out <- roc_proc_text(roc, "
    #' @name a
    #' @docType package
    NULL

    #' @name a-package
    #' @docType package
    NULL")
  
  alias_1 <- get_tag(out[[1]], "alias")$values
  expect_equal(alias_1, c("a", "a-package"))
  
  alias_2 <- get_tag(out[[2]], "alias")$values
  expect_equal(alias_2, c("a-package"))
})
