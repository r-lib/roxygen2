test_that("Source reference is included as comment", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @name a
    #' @title a
    NULL"
  )[[1]]

  expect_match(out$get_rd("backref"), "^% Please edit documentation in ")
})

test_that("Explicit @backref is included as comment", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @name a
    #' @title a
    #' @backref back/ref.file
    #' @backref root.file
    NULL"
  )[[1]]

  expect_equal(out$get_value("backref"), c("back/ref.file", "root.file"))
})
