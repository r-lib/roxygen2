#' @include list.R string.R functional.R
Rd <- function(partita) {
  ## TODO: param

  Rd.expression <- function(key, expression)
    sprintf('\\%s{%s}\n', key, expression)

  parse.default <- function(key, expression)
    cat(Rd.expression(key, expression))

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
    details <- do.call(paste, list(cdr(paragraphs), sep='\n\n'))
    print(expressions)
###     matter <- '[^\n]+'
###     words <- Curry(words.default, matter=matter)
###     nwords <- Curry(nwords.default, words=words)
###     word.ref <- Curry(word.ref.default, words=words)
###     strcar <- Curry(strcar.default, word.ref=word.ref)
###     strcdr <- Curry(strcdr.default, nwords=nwords, word.ref=word.ref)
###     description <- strcar(expressions)
###     details <- strcdr(expressions)
    parse.default('description', description)
    if (!is.null.string(details))
      parse.default('details', details)
  }

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
                  description=parse.description)

  parser <- function(key)
    if (is.null(f <- parsers[[key]])) parse.noop else f

  for (partitum in partita)
    for (key.value in zip.list(attributes(partitum)$names, partitum))
      do.call(parser(car(key.value)), cdr(key.value))
}
