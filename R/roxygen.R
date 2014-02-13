#' In-line documentation for R.
#'
#' Roxygen is a Doxygen-like documentation system for R; allowing
#' in-source specification of Rd files, collation and namespace
#' directives.
#'
#' If you have existing Rd files, check out the \code{Rd2roxygen} package
#' for a convenient way of converting Rd files to roxygen comments.
#'
#' @author
#' Hadley Wickham \email{h.wickham@@gmail.com},
#' Peter Danenberg \email{pcd@@roxygen.org},
#' Manuel Eugster \email{Manuel.Eugster@@stat.uni-muenchen.de}
#'
#' Maintainer: Hadley Wickham \email{h.wickham@@gmail.com}
#' @name roxygen
#' @docType package
#' @useDynLib roxygen2
#' @importFrom Rcpp sourceCpp
#' @examples
#' \dontrun{roxygenize('pkg')}
#' @seealso See \code{vignette("rd", package = "roxygen")} for an overview
#'   of the package, \code{vignette("rd", package = "roxygen")} for generating
#'   documenation, and \code{vignette("namespace", package = "roxygen")} for
#'   generating the namespace specification.
NULL
