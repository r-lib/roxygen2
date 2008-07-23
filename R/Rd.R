#' @include parse.R
#' @include list.R
#' @include string.R
#' @include roclet.R
roxygen()

#' Make an Rd roclet which parses the given files and writes the Rd
#' format to standard out (TODO: write to the file designated by
#' \code{@@name}). Requires the \code{@@name} parameter.
#'
#' Contains the member function \code{parse} which parses the result
#' of \code{parse.files}.
#'
#' @return Rd roclet
make.Rd.roclet <- function() {
  Rd.expression <- function(key, ...)
    sprintf('\\%s%s\n',
            key,
            Reduce.paste(function(expression)
                         sprintf('{%s}', trim(expression)),
                         c(...),
                         ''))

  parse.expression <- function(key, ...)
    cat(Rd.expression(key, c(...)))

  first.non.null <- function(...)
    append(NULL, c(...))[[1]]

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
      cat(strwrap(Rd.expression('usage',
          sprintf('%s(%s)', partitum$assignee, args)),
                  exdent=4),
          sep='\n')
    }
  }

  parse.usage <- function(partitum) {
    if (is.null(partitum$usage))
      parse.formals(partitum)
    else
      parse.expression('usage', partitum$usage)
  }

  pre.parse <- function(partitum) {
    assign.parent('params', nil, environment())
    parse.name(partitum)
    parse.usage(partitum)
  }

  post.parse <- function(partitum) {
    parse.arguments()
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
                                  'examples',
                                  'concept')

  roclet$register.parser('return',
                         function(key, expressions)
                         parse.expression('value', expressions))

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

  parse.description <- function(key, expressions) {
    paragraphs <- car(strsplit(car(expressions), '\n\n', fixed=TRUE))
    description <- car(paragraphs)
    details <- do.call(paste, append(cdr(paragraphs), list(sep='\n\n')))
    parse.expression('description', description)
    if (length(details) > 0 && !is.null.string(details))
      parse.expression('details', details)
  }

  roclet$register.parser('description', parse.description)

  params <- nil

  parse.param <- function(key, expression)
    assign.parent('params',
                  append(params, list(expression)),
                  environment())
    
  parse.params <- function()
    Reduce.paste(function(name.param)
                 Rd.expression('item',
                     car(name.param),
                     cadr(name.param)),
                 params,
                 '')

  parse.arguments <- function()
    if (length(params) > 0)
      parse.expression('arguments', parse.params())

  roclet$register.parser('param', parse.param)

  roclet
}
