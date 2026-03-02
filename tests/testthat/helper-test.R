expect_equivalent_rd <- function(out1, out2) {
  out1$sections$backref <- NULL
  out2$sections$backref <- NULL
  expect_equal(out1, out2)
}

expect_equal_strings <- function(s1, s2, ignore_ws = TRUE) {
  if (ignore_ws) {
    s1 <- gsub("\\s", "", s1, perl = TRUE)
    s2 <- gsub("\\s", "", s2, perl = TRUE)
  }
  expect_equal(s1, s2)
}

expect_parse_failure <- function(code) {
  (expect_condition(expect_null(code)))
}

local_package_copy <- function(path, env = caller_env(), set_version = TRUE) {
  temp_path <- withr::local_tempdir(.local_envir = env)

  file.copy(path, temp_path, recursive = TRUE)
  pkg_path <- dir(temp_path, full.names = TRUE)[[1]]

  if (set_version) {
    desc::desc_set(
      file = pkg_path,
      RoxygenNote = as.character(packageVersion("roxygen2"))
    )
  }

  normalizePath(pkg_path, winslash = "/")
}
