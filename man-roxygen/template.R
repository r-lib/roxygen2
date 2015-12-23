#' @section Template tags:
#'
#' Templates make it possible to substantially reduce documentation
#' duplication. A template is an \file{R} file in the \code{man-roxygen/} 
#' directory. It is processed with \code{\link[brew]{brew}} and then inserted 
#' into the roxygen block. You can run any R code with brew; you can
#' insert template variables with \code{<\%= varname \%>}.  
#'
#' \describe{
#'
#' \item{\code{@@template templateName}}{Insert named template in current
#'   location.}
#'
#' \item{\code{@@templateVar varname value}}{Set up variables for template
#'   use.}
#'
#' }
#'
#' Limitations:
#' 
#' \itemize{
#' \item Templates are not parsed recursively, so you can not include templates
#' from within other templates. 
#' 
#' \item Templates must be composed of complete tags - because all roxygen tags
#' are current block tags, they can not be used for inline insertions.
#' }
