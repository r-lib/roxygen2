# Preref parser table
# @TODO number parser?
preref.parsers <- new.env(parent=emptyenv())

# Srcref parser table
srcref.parsers <- new.env(parent=emptyenv())

#' Register a parser with a table
#'
#' @param table the table under which to register
#' @param key the key upon which to register
#' @param parser the parser callback to register;
#' a function taking \code{key} and \code{expression}
#' @return \code{NULL}
#' @keywords internal
register.parser <- function(table, key, parser)
  table[[key]] <- parser

#' Specifically register a preref parser
#'
#' @param key the key upon which to register
#' @param parser the parser callback to register;
#' a function taking \code{key} and \code{expression}
#' @return \code{NULL}
#' @seealso \code{\link{register.parser}}
#' @export
#' @keywords internal
register.preref.parser <- Curry(register.parser,
                                table=preref.parsers)

#' Specifically register a srcref parser
#'
#' @param key the key upon which to register
#' @param parser the parser callback to register;
#' a function taking \code{key} and \code{expression}
#' @return \code{NULL}
#' @seealso \code{\link{register.parser}}
#' @export
#' @keywords internal
register.srcref.parser <- Curry(register.parser,
                                table=srcref.parsers)

#' Register many parsers at once.
#'
#' @param table the table under which to register
#' @param parser the parser to register
#' @param \dots the keys upon which to register
#' @return \code{NULL}
#' @keywords internal
register.parsers <- function(table, parser, ...) {
  for (key in c(...))
    register.parser(table, key, parser)
}
  
#' Register many preref parsers at once.
#'
#' @param parser the parser to register
#' @param \dots the keys upon which to register
#' @return \code{NULL}
#' @export
#' @keywords internal
register.preref.parsers <- Curry(register.parsers,
                                 table=preref.parsers)

#' Register many srcref parsers at once.
#'
#' @param parser the parser to register
#' @param \dots the keys upon which to register
#' @return \code{NULL}
#' @export
#' @keywords internal
register.srcref.parsers <- Curry(register.parsers,
                                 table=srcref.parsers)
