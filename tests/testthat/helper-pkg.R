temp_copy_pkg <- function(pkg) {
  file.copy(normalizePath(pkg), tempdir(), recursive = TRUE)
  normalizePath(file.path(tempdir(), pkg))
}
