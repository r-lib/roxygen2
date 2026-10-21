test_that("roxygenize() can use a roclet defined in the package itself", {
  path <- withr::local_tempdir()
  write_lines(
    c(
      "Package: testselfroclet",
      "Version: 0.0.1",
      "Encoding: UTF-8",
      paste0("Config/roxygen2/version: ", packageVersion("roxygen2"))
    ),
    file.path(path, "DESCRIPTION")
  )
  dir.create(file.path(path, "R"))
  write_lines(
    c(
      "self_roclet <- function() roxygen2::roclet('self')",
      "roclet_process.roclet_self <- function(x, blocks, env, base_path) {",
      "  length(blocks)",
      "}",
      "roclet_output.roclet_self <- function(x, results, base_path, ...) {",
      "  writeLines(as.character(results), file.path(base_path, 'n-blocks'))",
      "}",
      "#' A function",
      "f <- function() 1"
    ),
    file.path(path, "R", "roclet.R")
  )
  write_lines(
    c(
      "export(self_roclet)",
      "S3method(roxygen2::roclet_process, roclet_self)",
      "S3method(roxygen2::roclet_output, roclet_self)"
    ),
    file.path(path, "NAMESPACE")
  )

  suppressMessages(roxygenize(path, "testselfroclet::self_roclet"))
  expect_equal(read_lines(file.path(path, "n-blocks")), "1")
})
