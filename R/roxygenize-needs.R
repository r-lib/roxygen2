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
  if (length(rd_files) == 0) {
    return(invisible(FALSE))
  }

  out_of_date <- character()

  for (rd_file in rd_files) {
    if (!made_by_roxygen(rd_file)) {
      next
    }

    source_files <- rd_backref_sources(rd_file, base_path)
    if (length(source_files) == 0) {
      next
    }

    rd_mtime <- file.mtime(rd_file)
    source_mtimes <- file.mtime(source_files)

    if (any(source_mtimes > rd_mtime)) {
      out_of_date <- c(out_of_date, basename(rd_file))
    }
  }

  if (length(out_of_date) > 0) {
    cli::cli_inform(c(
      "!" = "{length(out_of_date)} man page{?s} may be out of date:",
      set_names(out_of_date, "*")
    ))
    return(invisible(TRUE))
  }

  invisible(FALSE)
}

rd_backref_sources <- function(rd_file, base_path) {
  lines <- read_lines(rd_file, n = 10)
  backref_lines <- lines[grepl(
    "^%\\s*(Please edit documentation in|  )",
    lines
  )]
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
