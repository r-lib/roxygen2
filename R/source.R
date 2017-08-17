#' Source all files in a package.
#'
#' This is a wrapper around [pkgload::load_all]
#'
#' @param path Path to a package.
#' @return An environment, into which all R files in the directory were
#'   sourced.
#' @keywords internal
source_package <- function(path) {
  pkgload::load_all(path)$env
}

package_files <- function(path) {
  desc <- read_pkg_description(path)

  all <- normalizePath(r_files(path))

  collate <- scan(text = desc$Collate %||% "", what = "", sep = " ",
    quiet = TRUE)

  collate <- normalizePath(file.path(path, 'R', collate))

  rfiles <- c(collate, setdiff(all, collate))
  ignore_files(rfiles, path)
}

read_pkg_description <- function(path) {
  desc_path <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc_path)) stop("Can't find DESCRIPTION")

  read.description(desc_path)
}
