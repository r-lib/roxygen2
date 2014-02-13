pkg_env <- function() {
  env <- new.env(parent = globalenv())
  env$.packageName <- "roxygen2"
  env
}
