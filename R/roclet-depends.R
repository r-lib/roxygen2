#' @include parse-registry.R
NULL

register.preref.parsers(parse.value, 'depends')

#' Roclet: make Depend field in DESCRIPTION.
#'
#' Topologically sort R files and record in Depend field.
#'
#' Each \code{@@include} tag should specify the filename of one intrapackage
#' dependency; multiple \code{@@include} tags may be given.
#'
#' @family roclets
#' @return Rd roclet
#' @examples
#' #' `example-a.R', `example-b.R' and `example-c.R' reside
#' #' in the `example' directory, with dependencies
#' #' a -> {b, c}. This is `example-a.R'.
#' #' @@include example-b.R
#' #' @@include example-c.R
#' NULL
#'
#' roclet <- depends_roclet()
#' \dontrun{
#'   roc_proc(roclet, dir('example'))
#'   roc_out(roclet, dir('example'), "example")
#' }
#' @export
depends_roclet <- function() {
  new_roclet(list(), "depends")
}

#' @S3method roc_process depends
roc_process.depends <- function(roclet, partita, base_path) {
  idx <- c()
  for(partitum in partita)
    idx <- c(idx,unlist(partitum[names(partitum) == "depends"]))
  return(unique(idx))
}

#' @S3method roc_output depends
roc_output.depends <- function(roclet, results, base_path) {
  DESCRIPTION <- file.path(base_path, "DESCRIPTION")
  old <- read.description(DESCRIPTION)
  new <- old

  if (length(results) > 0) {
    results <- Filter(function(str) nchar(str) > 0, unlist(strsplit(results," +,*|,+ *")))
    x <- str_c(results, ",", collapse = "\n")
    if (nchar(x) > 1)
      new$Depends <- substr(x,1,nchar(x)-1)
    else new$Depends <- NULL
  } else new$Depends <- NULL
  write.description(new, DESCRIPTION)
  
  if (!identical(old, read.description(DESCRIPTION))) {
    cat('Updating Depends directive in ', DESCRIPTION, "\n")
  }
}


