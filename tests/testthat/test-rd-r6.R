test_that("integration test", {
  path <- local_package_copy(test_path("testR6"))
  suppressMessages(roxygenise(path))
  withr::defer(pkgload::unload("testR6"))

  rd_files <- sort(dir(file.path(path, "man"), full.names = TRUE))
  for (rd_file in rd_files) {
    expect_snapshot(cat(read_lines(rd_file), sep = "\n"))
    expect_silent(chk <- tools::checkRd(rd_file))
    expect_equal(length(chk), 0L)
  }
})

test_that("multiple R6 classes in one topic (@rdname) produce valid Rd", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #' @name shared
    NULL

    #' @rdname shared
    A <- R6::R6Class('A',
      public = list(
        #' @field x Field x.
        x = NULL,
        #' @description Method a.
        meth_a = function() {}
      )
    )

    #' @rdname shared
    B <- R6::R6Class('B',
      public = list(
        #' @field y Field y.
        y = NULL,
        #' @description Method b.
        meth_b = function() {}
      )
    )
    "
  )[[1]]

  rd <- out$get_rd("rawRd")
  expect_match(rd, "\\item{\\code{x}}{Field x.}", fixed = TRUE)
  expect_match(rd, "\\item{\\code{y}}{Field y.}", fixed = TRUE)
  expect_match(rd, "A$meth_a", fixed = TRUE)
  expect_match(rd, "B$meth_b", fixed = TRUE)
})
