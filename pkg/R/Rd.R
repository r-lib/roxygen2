#' @include parse.R
#' @include list.R
#' @include string.R
#' @include roclet.R
roxygen()

#' Make an Rd roclet which parses the given files and writes the Rd
#' format to standard out (TODO: write to the file designated by
#' \code{@@name}). Tries to guess \name and \usage unless explicitly
#' given.
#'
#' Contains the member function \code{parse} which parses the result
#' of \code{parse.files}.
#'
#' @param stdout whether to cat to standard out (e.g. for testing)
#' @return Rd roclet
make.Rd.roclet <- function(stdout=FALSE) {
  #' Translate a key and expressions into an Rd expression;
  #' multiple expressions take their own braces.
  #' @param key the expression's key
  #' @param \dots the arguments
  #' @return A string containing the key and arguments
  #' in LaTeX-like gestalt.
  Rd.expression <- function(key, ...)
    sprintf('\\%s%s\n',
            key,
            Reduce.paste(function(expression)
                         sprintf('{%s}', trim(expression)),
                         c(...),
                         ''))

  #' Push the Rd-expression to standard out (or current
  #' sink).
  #' @param key the expression's key
  #' @param \dots the arguments
  #' @return \code{NULL}
  parse.expression <- function(key, ...)
    cat(Rd.expression(key, c(...)))

  #' Reconstruct the \name directive from amongst
  #' \code{@@name}, \code{@@setMethod}, \code{@@setClass},
  #' \code{@@setGeneric}, \code{@@assignee}, etc.
  #' @param partitum the pre-parsed elements
  #' @return \code{NULL}
  parse.name <- function(partitum) {
    name <- partitum$name
    assignee <- partitum$assignee
    S4 <- first.non.null(partitum$S4class,
                         partitum$S4method,
                         partitum$S4generic)
    name <- first.non.null(name, assignee, S4)
    ## sink(name)
    if (!is.null(name))
      parse.expression('name', name)
  }
  
  #' Turn a list of formal arguments into a human-readable
  #' function-like.
  #' @param partitum the pre-parsed elements
  #' @return \code{NULL}
  parse.formals <- function(partitum) {
    formals <- partitum$formals
    if (!is.null(formals)) {
      name.defaults <- zip.c(names(formals), formals)
      args <-
        do.call(paste, c(Map(function(name.default) {
          name <- car(name.default)
          default <- cadr(name.default)
          if (is.null.string(default))
            name
          else
            sprintf('%s=%s', name, default)
        },
                             name.defaults),
                         sep=', '))
      parse.expression('usage',
          do.call(paste,
                  c(as.list(strwrap(sprintf('%s(%s)',
                                            partitum$assignee,
                                            args),
                                    exdent=4)),
                    sep='\n')))
    }
  }

  #' Prefer explicit \code{@@usage} to a \code{@@formals} list.
  #' @param partitum the pre-parsed elements
  #' @return \code{NULL}
  parse.usage <- function(partitum) {
    if (is.null(partitum$usage))
      parse.formals(partitum)
    else
      parse.expression('usage', partitum$usage)
  }

  #' Reset params; parse name and usage.
  #' @param partitum the pre-parsed elements
  #' @return \code{NULL}
  pre.parse <- function(partitum) {
    assign.parent('params', NULL, environment())
    assign.parent('examples', NULL, environment())
    parse.name(partitum)
    parse.usage(partitum)
  }

  #' Parse params.
  #' @param partitum the pre-parsed elements
  #' @return \code{NULL}
  post.parse <- function(partitum) {
    parse.arguments()
    parse.examples(partitum)
    ## sink(NULL)
  }

  roclet <- make.roclet(parse.expression,
                        pre.parse,
                        post.parse)

  roclet$register.default.parsers('title',
                                  'references',
                                  'note',
                                  'author',
                                  'seealso',
                                  'concept')

  roclet$register.parser('return',
                         function(key, expressions)
                         parse.expression('value', expressions))

  #' Split a plural into its constituent singulars.
  #' @param key the singular key
  #' @param expressions the plurality of expressions
  #' @return \code{NULL}
  parse.split <- function(key, expressions) {
    expression <- strcar(expressions)
    rest <- strcdr(expressions)
    parse.expression(key, expression)
    if (!is.null.string(rest))
      parse.split(key, rest)
  }

  roclet$register.parser('aliases',
                         function(key, expressions)
                         parse.split('alias', expressions))

  roclet$register.parser('keywords',
                         function(key, expressions)
                         parse.split('keyword', expressions))

  #' Split the introductory matter into its description followed
  #' by details (separated by a blank line).
  #' @param key ignored
  #' @param expressions the to-be-parsed description and details
  #' @return \code{NULL}
  parse.description <- function(key, expressions) {
    paragraphs <- car(strsplit(car(expressions), '\n\n', fixed=TRUE))
    description <- car(paragraphs)
    details <- do.call(paste, append(cdr(paragraphs), list(sep='\n\n')))
    parse.expression('description', description)
    if (length(details) > 0 && !is.null.string(details))
      parse.expression('details', details)
  }

  roclet$register.parser('description', parse.description)

  params <- NULL

  #' Add a parameter to the global param list.
  #' @param key ignored
  #' @param expression the parameter
  #' @return \code{NULL}
  parse.param <- function(key, expression)
    assign.parent('params',
                  append(params, list(expression)),
                  environment())
    
  #' Reduce and paste together the various parameters
  #' in an Rd-readable list (with \code{\\item}s, etc.).
  #' @param name.param name-param pair
  #' @return A list of Rd-readable expressions
  parse.params <- function()
    Reduce.paste(function(name.param)
                 Rd.expression('item',
                     car(name.param),
                     cadr(name.param)),
                 params,
                 '')

  #' Paste and label the Rd-readable expressions
  #' returned by \code{parse.params}.
  #' @return \code{NULL}
  parse.arguments <- function()
    if (length(params) > 0)
      parse.expression('arguments', parse.params())

  roclet$register.parser('param', parse.param)

  examples <- NULL

  #' Parse individual \code{@@example} clauses by adding the
  #' pointed-to file to a global store.
  #' @param key ignored
  #' @param expression the file containing the example(s)
  #' @return \code{NULL}
  parse.example <- function(key, expression)
    assign.parent('examples',
                  append(examples, expression),
                  environment())

  #' If \code{@@examples} is provided, use that; otherwise, concatenate
  #' the files pointed to by each \code{@@example}.
  #' @param partitum the parsed elements
  #' @return \code{NULL}
  parse.examples <- function(partitum) {
    if (!is.null(partitum$examples))
      parse.expression('examples', partitum$examples)
    else {
      examples <- Reduce(c, Map(function(file)
                                tryCatch(readLines(trim(file)),
                                         error=function(e) NULL),
                                examples),
                         NULL)
      if (!is.null(examples))
        parse.expression('examples',
            do.call(paste, c(as.list(examples), sep='\n')))
    }
  }

  roclet$register.parser('example', parse.example)

  roclet
}
