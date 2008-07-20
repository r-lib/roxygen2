#' @include list.R
#' @include string.R
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

  pre.parse <- function(partitum)
    assign.parent('params', nil, environment())

  post.parse <- function(partitum)
    parse.arguments()

  roclet <- make.roclet(parse.expression,
                        pre.parse,
                        post.parse)

  roclet$register.default.parsers('name',
                                  'title',
                                  'usage',
                                  'value',
                                  'references',
                                  'note',
                                  'author',
                                  'seealso',
                                  'examples',
                                  'concept')

  parse.split <- function(key, expressions) {
    expression <- strcar(expressions)
    rest <- strcdr(expressions)
    parse.expression(key, expression)
    if (!is.null.string(rest))
      parse.split(key, rest)
  }

  roclet$register.parsers(parse.split,
                          'keyword',
                          'alias')

  parse.description <- function(key, expressions) {
    paragraphs <- car(strsplit(car(expressions), '\n\n', fixed=T))
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
