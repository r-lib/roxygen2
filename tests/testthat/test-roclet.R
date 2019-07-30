context("roclet utilities")

test_that("input validation", {
  expect_error(roc_proc_text(list(), ""))
})

test_that("combining roclets", {
  out <- roc_proc_text(list(namespace_roclet(), rd_roclet()), "
                       #' @title a
                       #' @references test
                       #' @note test
                       #' @author test
                       #' @seealso test
                       #' @encoding test
                       #' @name a
                       #' @export
                       NULL")

  expect_equal(length(out), 2L)

  # namespace
  out_namespace <- out[[1L]]
  expect_equal(length(out_namespace), 1L)
  expect_equal(out_namespace, "export(a)")

  # rd
  out_rd_result <- out[[2L]]
  expect_equal(length(out_rd_result), 1L) # 1 entry only
  out_rd <- out_rd_result[[1L]] # pick that entry

  expect_equal(get_tag(out_rd, "title")$values, "a")
  expect_equal(get_tag(out_rd, "name")$values, "a")
  expect_equal(get_tag(out_rd, "references")$values, "test")
  expect_equal(get_tag(out_rd, "note")$values, "test")
  expect_equal(get_tag(out_rd, "seealso")$values, "test")
  expect_equal(get_tag(out_rd, "encoding")$values, "test")
  expect_equal(get_tag(out_rd, "author")$values, "test")
})
