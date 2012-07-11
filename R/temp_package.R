#' Create a temporary package
#' 
#' Create a temporary package from at least a DESCRIPTION file.
#' This creates a tempdir, then copies across the relevant files.
#' Note the DESCRIPTION is all that's needed for a successful 
#' \code{R CMD BUILD}. This is very useful during testing of roxygen2.
#' 
#' @param description.file the path to a DESCRIPTION file
#' @param namespace.file [optional] the path to a NAMESPACE file
#' @param R.files [optional] vector of paths to .R source files
#' @param Rd.files [optional] vector of paths to .Rd documentation files
#' 
#' @return the path to the package
#' 
#' @noRd
#' 
#' @examples
#' df <- file.path(path.package("roxygen2"), "tests", "description-example.txt")
#' nf <- "namespace-example1.txt"
#' pkg <- temppackage(df, nf)
temppackage <- function(description.file, namespace.file=NULL, R.files=NULL, Rd.files=NULL) {
  !missing(description.file) || stop("Must specify at least the DESCRIPTION file")
  length(namespace.file) == 1 && file.exists(namespace.file) || stop("Must specify either NULL, or a single, valid path to a NAMESPACE file")
  is.null(R.files) || all(file.exists(R.files)) || stop("Must specify either NULL, or valid path(s) to a .R source file(s)")
  is.null(Rd.files) || all(file.exists(Rd.files)) || stop("Must specify either NULL, or valid path(s) to a .Rd documentation file(s)")
  
  dir <- tempfile()
  dir.create(dir)
  
  file.copy(description.file, file.path(dir, "DESCRIPTION"))
  if( !is.null(namespace.file) ) file.copy(namespace.file, file.path(dir, "NAMESPACE"))
  
  if( length(R.files) > 0 ) {
    dir.create( file.path(dir, "R") )
    for(i in seq(along=R.files)) {
      file.copy(R.files[i], file.path(dir, "R", basename(R.files[i])))
    }
  }
  if( length(Rd.files) > 0 ) {
    dir.create( file.path(dir, "Rd") )
    for(i in seq(along=Rd.files)) {
      file.copy(Rd.files[i], file.path(dir, "Rd", basename(Rd.files[i])))
    }
  }
  return( dir )
}

