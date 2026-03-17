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
