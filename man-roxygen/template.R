#' @section Template tags:
#'
#' Templates make it possible to substantially reduce documentation
#' duplication. A template is an \file{R} file processed with
#' \code{\link[brew]{brew}} and then inserted into the roxygen block.
#' Templates can use variables, accessible from within brew with 
#' \code{<=\% varname =>}.  
#'
#' Templates are not parsed recursively, so you can not include templates
#' from within other templates.
#'
#' Templates must be composed of complete tags - becuase all roxygen tags
#' are current block tags, they can not be used for inline insertions.
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
