#' Parse a source file containing roxygen directives.
#'
#' @param file string naming file to be parsed
#' @return List containing parsed directives
#' @keywords internal
#' @export
parse.file <- function(file) {
  srcfile <- srcfile(file)
  
  res <- try(cached.parse.srcfile(srcfile), silent = TRUE)
  if (inherits(res, "try-error")) {
    stop("Can't parse ", file, "\n", res, call. = FALSE)
  }
  res
}

parse.srcfile <- function(srcfile) {
  src_refs <- attributes(parse(srcfile$filename, srcfile = srcfile))$srcref
  pre_refs <- prerefs(srcfile, src_refs)

  if (length(src_refs) == 0) return(list())
  
  src_parsed <- lapply(src_refs, cached.parse.ref)
  pre_parsed <- lapply(pre_refs, cached.parse.ref)
  mapply(c, src_parsed, pre_parsed, SIMPLIFY = FALSE)
}
cached.parse.srcfile <- memoize(parse.srcfile)

#' Parse many files at one.
#'
#' @param \dots files to be parsed
#' @return List containing parsed directives
#' @seealso \code{\link{parse.file}}
#' @keywords internal
#' @export
parse.files <- function(paths) {
  unlist(lapply(paths, parse.file), recursive = FALSE)
}
  
#' Text-parsing hack using tempfiles for more facility.
#'
#' @param text stringr containing text to be parsed
#' @return The parse tree
#' @keywords internal
#' @export
parse.text <- function(text) {
  file <- tempfile()
  writeLines(text, file)
  on.exit(unlink(file))
  parse.file(file)
}
