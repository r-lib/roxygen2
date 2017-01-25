#' Source all files in a package.
#'
#' This is a simple attempt to load code in a package used by
#' [roxygenize]. It will work with simple packages, but fail if
#' there are compiled files, data files, etc. In that case, it's better to
#' use [devtools::document].
#'
#' @param path Path to a package.
#' @return An environment, into which all R files in the directory were
#'   sourced.
#' @keywords internal
source_package <- function(path) {
  r_path <- file.path(path, "R")
  if (!file.exists(r_path)) stop("Can't find R/ directory", call. = FALSE)

  old_dir <- setwd(r_path)
  on.exit(setwd(old_dir))

  env <- new.env(parent = globalenv())
  methods::setPackageName("roxygen_devtest", env)

  load_pkg_dependencies(path)

  desc <- read_pkg_description(path)
  paths <- package_files(path)
  lapply(paths, sys_source, envir = env, fileEncoding = desc$Encoding %||% "UTF-8")

  env
}

sys_source <- function(file, envir = baseenv(), fileEncoding = "UTF-8") {
  exprs <- parse(text = read_lines_enc(file, file_encoding = fileEncoding))
  for (expr in exprs) {
    eval(expr, envir = envir)
  }
  invisible()
}

# Assume that the package has already been loaded by other means
# (e.g. build and reload)
loaded_package <- function(path) {
  desc <- file.path(path, "DESCRIPTION")
  stopifnot(file.exists(desc))

  package <- read.dcf(desc, fields = "Package")[[1, 1]]
  asNamespace(package)
}



load_pkg_dependencies <- function(path) {
  desc <- read_pkg_description(path)

  pkgs <- paste(c(desc$Depends, desc$Imports), collapse = ", ")
  if (pkgs == "") return()

  pkgs <- str_replace_all(pkgs, "\\(.*?\\)", "")
  pkgs <- str_split(pkgs, ",")[[1]]
  pkgs <- str_trim(pkgs)
  lapply(pkgs[pkgs != "R"], require, character.only = TRUE)
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
