#' Parse a source file containing roxygen directives.
#'
#' @param file string naming file to be parsed
#' @return List containing parsed directives
#' @keywords internal
#' @export
parse.file <- function(file, env, env_hash) {
  srcfile <- srcfile(file)
  
  parse_cache$compute(c(env_hash, readLines(file, warn = FALSE)), {
    src_refs <- attributes(parse(srcfile$filename, srcfile = srcfile))$srcref
    pre_refs <- prerefs(srcfile, src_refs)

    if (length(src_refs) == 0) return(list())

    src_parsed <- lapply(src_refs, parse.srcref, env = env)
    pre_parsed <- lapply(pre_refs, parse.preref)

    stopifnot(length(src_parsed) == length(pre_parsed))

    mapply(c, src_parsed, pre_parsed, SIMPLIFY = FALSE)    
  })
}

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
  
  setPackageName("roxygen_destest", env)
  lapply(paths, sys.source, chdir = TRUE, envir = env)
  
  unlist(lapply(paths, parse.file, env = env, env_hash = env_hash), 
    recursive = FALSE)
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


#' convert package dependency list to vector
#'
#' Parse the package dependencies in a DESCRIPTION file (from in the
#' Depends, Imports or Suggests fields), from a comma and possibly newline
#' separated, possibly versioned (>= X.Y.Z) character(1) vector, and
#' return a character vector of package names, 1 per element. 
#' The return values are unversioned, names are versioned. see examples.
#' 
#' @param pkgs a character(1) of package names. see \code{read.description(...)$Depends}
#'  and examples
#' @param exclude.R logical: if \code{TRUE}, then don't return the \R dependency
#'  often found in the Depends field.
#' 
#' @return a character vector of package names, named by either the package name,
#'  or of the versioned package name, depedning on the input. see datapoint 2 in
#'  the examples
#' 
#' @author Mark Cowley, 2012-07-09
#' @noRd
#' 
#' @examples
#' # pkgs <- paste(c(desc$Depends, desc$Imports), collapse = ", ")
#' pkgs <- "digest, stringr (>= 0.5),\ntools,\nbrew"
#' roxygen2:::parse.dependencies(pkgs)
#' #   digest  stringr (>= 0.5)    tools    brew 
#' # "digest"         "stringr"  "tools"  "brew" 
parse.dependencies <- function(pkgs, exclude.R = TRUE) {
  if (pkgs != "") {
    pkgs <- strsplit(pkgs, ",")[[1]]
    pkgs <- gsub("^\\s+|\\s+$", "", pkgs)
    pkg.ver <- pkgs
    pkgs <- gsub("\\s*\\(.*?\\)", "", pkgs)
    names(pkgs) <- pkg.ver
    if (exclude.R) {
      pkgs <- pkgs[pkgs != "R"]
    }
  }
  return( pkgs )
}
