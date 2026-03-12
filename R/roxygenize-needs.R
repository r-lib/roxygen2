#' Check if documentation needs to be updated
#'
#' @description
#' A lightweight check that compares modification times of `.Rd` files in
#' `man/` with the source files listed in their backrefs. This is much faster
#' than running [roxygenize()] but can suffer from both false negatives (e.g.
#' if an inherited documentation topic has changed) and false positives (e.g.
#' if a source file was modified but the change doesn't affect the
#' documentation).
#'
#' @inheritParams roxygenize
#' @return A logical value, invisibly. `TRUE` if any man pages appear
#'   to be out of date; `FALSE` otherwise.
#' @export
#' @examples
#' \dontrun{
#' needs_roxygenize()
#' }
needs_roxygenize <- function(package.dir = ".") {
  base_path <- normalizePath(package.dir)
  man_path <- file.path(base_path, "man")
  rd_files <- sort(dir(man_path, pattern = "\\.Rd$", full.names = TRUE))
  outdated <- map_lgl(rd_files, rd_outdated, base_path = base_path)
  out_of_date <- basename(rd_files[outdated])

  if (length(out_of_date) > 0) {
    cli::cli_inform(c(
      "!" = "{length(out_of_date)} man page{?s} may be out of date:",
      set_names(out_of_date, "*")
    ))
    return(invisible(TRUE))
  }

  invisible(FALSE)
}

rd_outdated <- function(rd_file, base_path) {
  lines <- read_lines(rd_file, n = 10)
  source_files <- rd_backref_sources(lines, base_path)
  if (length(source_files) == 0) {
    return(FALSE)
  }

  if (!all(file.exists(source_files))) {
    return(TRUE)
  }

  rd_mtime <- file.mtime(rd_file)
  source_mtimes <- file.mtime(source_files)
  any(source_mtimes > rd_mtime)
}

rd_backref_sources <- function(lines, base_path) {
  if (!check_made_by(lines[[1]])) {
    return(character())
  }

  backref_regexp <- "^%\\s*(Please edit documentation in|  )"
  backref_lines <- lines[grepl(backref_regexp, lines)]
  if (length(backref_lines) == 0) {
    return(character())
  }

  text <- paste(backref_lines, collapse = " ")
  text <- sub("^%\\s*Please edit documentation in\\s+", "", text)
  # Clean up continuation line prefixes
  text <- gsub("\\s*%\\s*", " ", text)

  files <- strsplit(text, ",\\s*")[[1]]
  files <- trimws(files)
  files <- files[nzchar(files)]

  file.path(base_path, files)
}
