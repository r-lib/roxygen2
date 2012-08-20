#' @include parse-registry.R
NULL

register.preref.parsers(parse.value, 'title')

#' Roclet: make INDEX.
#' 
#' This roclet automates the production of an \file{INDEX} file, 
#' see \cite{Writing R Extensions}
#' (\url{http://cran.r-project.org/doc/manuals/R-exts.pdf}) for details.
#'
#' @section Tags:
#'
#' There are four tags for exporting objects from the package:
#' 
#' \describe{
#'
#' \item{\code{@@title}}{Roxygen guesses the directive: \code{export} for 
#'   functions, \code{exportMethod} for S4 methods, \code{S3method} for S3
#'   methods, \code{exportClass} for S4 classes.
#'   
#'   This is the only directive you should need for documented function,
#'   the other directives are useful if you want to export (e.g.) methods
#'   but not document them.} 
#'
#' \item{\code{@@export f g ...}}{overrides auto-detection and 
#'   produces multiple export directives: \code{export(f)}, \code{export(g)} 
#'   ...}
#'    
#' }
#'
#' Only unique directives are saved to the \file{INDEX} file, so you can
#' repeat them as needed to maintain a close link between the functions where
#' they are needed and the index file..
#' 
#' @family roclets
#' @examples
#' #' An example file, example.R, which imports
#' #' packages foo and bar
#' #' @@import foo bar
#' NULL
#'
#' #' An exportable function
#' #' @@export
#' fun <- function() {}
#'
#' roclet <- index_roclet()
#' \dontrun{roc_proc(roclet, "example.R")}
#' \dontrun{roc_out(roclet, "example.R", ".")}
#' @export
#' @aliases export exportClass exportMethod S3method import importFrom
#'   importClassesFrom importMethodsFrom
index_roclet <- function() {
  new_roclet(list, "index")
}

#' @S3method roc_process index
roc_process.index <- function(roclet, partita, base_path) {
  idx <- character()
  for (partitum in partita) {
    if (!is.null(partitum$export)) {
        name <- partitum$name %||% partitum$assignee %||% partitum$S4class %||% partitum$S4method %||% partitum$S4generic
        if (is.null(name)) {
          if (length(partitum$assignee) == 1) {
            name <- partitum$assignee
          } else {
            warning('Unassociated export directive', call. = FALSE)            
            next
          }
        }
        nblanks <- 23 - str_length(name)
        if(nblanks > 0)
          blanks <- str_dup(' ',nblanks)
        else
          blanks <- str_c('\n',str_dup(' ',23))
        if(!is.null(partitum$title)) {
          idx <- c(idx,str_c(name,blanks,str_trim(partitum$title)))
        } else if(!is.null(partitum$introduction)) {
          idx <- c(idx,str_c(name,blanks,str_trim(strsplit(partitum$introduction, '\n\n', fixed=TRUE)[[1]])[1]))
        } else warning(str_c(name,' has no title'), call. = FALSE)
      }
  }
  return(sort(unique(idx)))
}

#' @S3method roc_output index
roc_output.index <- function(roclet, results, base_path) { 
  INDEX <- file.path(base_path, "INDEX")
  old <- if (file.exists(INDEX)) readLines(INDEX) else ""
  if (!identical(results, old)) {
    cat("Updating index directives\n")
    writeLines(results, INDEX)
  }
}
