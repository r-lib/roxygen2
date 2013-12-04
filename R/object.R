#' Constructors for S3 object to represent R object.
#' 
#' These objects are usually created by the parsers, but it is also
#' useful to generate them by hand for testing.
#' 
#' @param subclass This is an abstract class so this must be provided. 
#'   Currently this is one of "function", "s4generic", "s4class", "s4method",
#'   "rcclass", or "data".
#' @param name Name of the object being documented
#' @param value The object itself. 
#' @param alias Used for \code{\link{setClass}} and \code{\link{setRefClass}}
#'   to capture the name of the created object.
#' @export
#' @keywords internal
object <- function(subclass, name, value, alias = NULL) {
  structure(list(
    name = name, 
    value = value,
    alias = alias), 
  class = c(subclass, "object"))
}

hash_object <- function(x) {
  if (is.null(x)) return(x)
  
  # If a function, hash based on source
  if (is.function(x$value)) {
    x$value <- deparse(body(x$value))
  }
  
  suppressWarnings(digest(x))
}
