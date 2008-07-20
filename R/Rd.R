#' @include list.R string.R functional.R
Rd <- function(partita) {
  Rd.expression <- function(key, ...)
    sprintf('\\%s%s\n',
            key,
            Reduce.paste(function(expression)
                         sprintf('{%s}', trim(expression)),
                         c(...),
                         NIL.STRING))

  parse.default <- function(key, ...)
    cat(Rd.expression(key, c(...)))

  parse.name <- Curry(parse.default, key='name')

  parse.title <- Curry(parse.default, key='title')

  parse.usage <- Curry(parse.default, key='usage')

  parse.return <- Curry(parse.default, key='value')

  parse.references <- Curry(parse.default, key='references')

  parse.note <- Curry(parse.default, key='note')

  parse.author <- Curry(parse.default, key='author')

  parse.seealso <- Curry(parse.default, key='seealso')

  parse.examples <- Curry(parse.default, key='examples')

  parse.concept <- Curry(parse.default, key='concept')

  parse.split <- function(key, expressions) {
    expression <- strcar(expressions)
    rest <- strcdr(expressions)
    parse.default(key, expression)
    if (!is.null.string(rest))
      parse.split(key, rest)
  }

  parse.keywords <- Curry(parse.split, key='keyword')

  parse.aliases <- Curry(parse.split, key='alias')

  parse.description <- function(expressions) {
    paragraphs <- car(strsplit(expressions, '\n\n', fixed=T))
    description <- car(paragraphs)
    details <- do.call(paste, append(cdr(paragraphs), list(sep='\n\n')))
    parse.default('description', description)
    if (length(details) > 0 && !is.null.string(details))
      parse.default('details', details)
  }

  params <- nil

  parse.param <- function(expression)
    ## Hack to access persistent `params'
    assign('params',
           append(params, list(expression)),
           envir=parent.env(environment()))
    
  parse.params <- function(params)
    Reduce.paste(function(name.param)
                 Rd.expression('item',
                     car(name.param),
                     cadr(name.param)),
                 params,
                 '')

  parse.arguments <- function(params)
    if (length(params) > 0)
      parse.default('arguments', parse.params(params))

  parse.noop <- function(expression) NULL

  parsers <- list(name=parse.name,
                  title=parse.title,
                  usage=parse.usage,
                  return=parse.return,
                  references=parse.references,
                  note=parse.note,
                  author=parse.author,
                  seealso=parse.seealso,
                  examples=parse.examples,
                  concept=parse.concept,
                  aliases=parse.aliases,
                  keywords=parse.keywords,
                  description=parse.description,
                  param=parse.param)

  parser <- function(key)
    if (is.null(f <- parsers[[key]])) parse.noop else f

  ## Parse the rest first for side-effects (i.e. parameter-gathering).
  for (partitum in partita)
    for (key.value in zip.list(attributes(partitum)$names, partitum))
      do.call(parser(car(key.value)), cdr(key.value))

  ## Then parse the arguments.
  parse.arguments(params)
}
