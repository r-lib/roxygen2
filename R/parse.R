#' Parse many files at once.
#'
#' @param \dots files to be parsed
#' @return List containing parsed directives
#' @seealso \code{\link{parse.file}}
#' @keywords internal
#' @export
#' @importFrom digest digest
parse.files <- function(paths) {
  # Source all files into their own environment so that parsing code can
  # access them.
  env <- new.env(parent = parent.env(globalenv()))
  env_hash <- suppressWarnings(digest(env))
  setPackageName("roxygen_devtest", env)
  
  lapply(paths, sys.source, chdir = TRUE, envir = env)
  
  parsed <- lapply(paths, parse.file, env = env, env_hash = env_hash)
  unlist(parsed, recursive = FALSE)
}

#' Parse a source file containing roxygen directives.
#'
#' @param file string naming file to be parsed
#' @return List containing parsed directives
#' @keywords internal
#' @export
parse.file <- function(file, env, env_hash) {
  srcfile <- srcfile(file)
  
  lines <- readLines(file, warn = FALSE)
  hash <- c(env_hash, lines)
  
  if (parse_cache$has_key(hash)) {
    return(parse_cache$get_key(hash))
  }
  
  src_refs <- attributes(parse(srcfile$filename, srcfile = srcfile))$srcref
  pre_refs <- prerefs(srcfile, src_refs)

  if (length(src_refs) == 0) return(list())

  src_parsed <- lapply(src_refs, parse.srcref, env = env)
  pre_parsed <- lapply(pre_refs, parse.preref)

  stopifnot(length(src_parsed) == length(pre_parsed))

  partita <- mapply(c, src_parsed, pre_parsed, SIMPLIFY = FALSE)    
  parse_cache$set_key(hash, partita)
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
  parse.files(file)
}
