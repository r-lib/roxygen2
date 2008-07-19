#' @include string.R list.R
LINE.DELIMITER <- '#\' '
TAG.DELIMITER <- '@'

#' Comment blocks (possibly null) that precede a file's expressions.
prerefs <- function(srcfile, srcrefs) {
  length.line <- function(lineno)
    nchar(getSrcLines(srcfile, lineno, lineno))

  pair.preref <- function(pair) {
    start <- car(pair)
    end <- cadr(pair)
    structure(srcref(srcfile, c(start, 1, end, length.line(end))),
              class='preref')
  }

  lines <- unlist(Map(function(srcref)
                      c(car(srcref) - 1,
                        caddr(srcref) + 1),
                      srcrefs))
  pairs <- pairwise(c(1, lines))
  Map(pair.preref, pairs)
}

## preref parsers

parse.message <- function(key, message)
  sprintf('@%s %s.', key, message)

parse.error <- function(key, message)
  stop(parse.message(key, message))

parse.warning <- function(key, message)
  warning(parse.message(key, message))

parse.element <- function(element) {
  tag <- strcar(element)
  rest <- strcdr(element)
  parser <- parser.preref(tag)
  do.call(parser, list(rest))
}

parse.description <- function(expression)
  list(description=expression)

parse.default <- function(key, rest)
  as.list(structure(rest, names=key))

parse.preref <- function(key, rest) {
  parse.warning(sprintf('<%s>', key), 'is an unknown key')
  parse.default(key, rest)
}

## Possibly NA; in which case, the Roclets can do something more
## sophisticated with the srcref.
parse.export <- Curry(parse.default, key='export')

parse.value <- function(key, rest) {
  if (is.null.string(rest))
    parse.error(key, 'requires a value')
  else
    parse.default(key, rest)
}
  
parse.prototype <- Curry(parse.value, key='prototype')

parse.exportClass <- Curry(parse.value, key='exportClass')

parse.exportMethod <- Curry(parse.value, key='exportMethod')

parse.exportPattern <- Curry(parse.value, key='exportPattern')

parse.S3method <- Curry(parse.value, key='S3method')

parse.import <- Curry(parse.value, key='import')

parse.importFrom <- Curry(parse.value, key='importFrom')

parse.importClassesFrom <- Curry(parse.value, key='importClassesFrom')

parse.importMethodsFrom <- Curry(parse.value, key='importMethodsFrom')

## Rd stuff

parse.name <- Curry(parse.value, key='name')

parse.aliases <- Curry(parse.value, key='aliases')

parse.title <- Curry(parse.value, key='title')

parse.usage <- Curry(parse.value, key='usage')

parse.references <- Curry(parse.value, key='references')

parse.concept <- Curry(parse.value, key='concept')

parse.note <- Curry(parse.value, key='note')

parse.seealso <- Curry(parse.value, key='seealso')

parse.examples <- Curry(parse.value, key='examples')

parse.keywords <- Curry(parse.value, key='keywords')

parse.return <- Curry(parse.value, key='return')

parse.author <- Curry(parse.value, key='author')

parse.name.description <- function(key, rest) {
  name <- strcar(rest)
  rest <- strcdr(rest)
  if (is.null.string(name))
    parse.error(key, 'requires a name and description')
  else
    as.list(structure(list(list(name=name,
                                description=rest)),
                      names=key))
}

parse.slot <- Curry(parse.name.description, key='slot')

parse.param <- Curry(parse.name.description, key='param')

parse.name.internal <- function(key, name) {
  if (is.null.string(name))
    parse.error(key, 'requires a name')
  parse.default(key, strcar(name))
}

parse.S3class <- Curry(parse.name.internal, key='S3class')

parse.returnType <- Curry(parse.name.internal, key='returnType')

parse.toggle <- function(key, rest)
  as.list(structure(T, names=key))

parse.listObject <- Curry(parse.toggle, key='listObject')

parse.attributeObject <- Curry(parse.toggle, key='attributeObject')

parse.environmentObject <- Curry(parse.toggle, key='environmentObject')

## srcref parsers

parse.srcref <- function(...) nil

parse.setClass <- function(expression)
  list(class=cadr(car(expression)))

parse.setGeneric <- function(expression)
  list(generic=cadr(car(expression)))

parse.setMethod <- function(expression)
  list(method=cadr(car(expression)),
       signature=caddr(car(expression)))

## Parser lookup

parser.default <- function(key, default) {
  f <- ls(1, pattern=sprintf('parse.%s', trim(key)))[1]
  if (is.na(f)) Curry(default, key=key) else f
}

parser.preref <- Curry(parser.default, default=parse.preref)

parser.srcref <- Curry(parser.default, default=parse.srcref)

## File -> {src,pre}ref mapping

parse.ref <- function(x, ...)
  UseMethod('parse.ref')

parse.ref.list <- function(preref.srcref)
  append(parse.ref(car(preref.srcref)),
         parse.ref(cadr(preref.srcref)))

parse.ref.preref <- function(preref) {
  lines <- getSrcLines(attributes(preref)$srcfile,
                       car(preref),
                       caddr(preref))
  delimited.lines <-
    Filter(function(line) grep(LINE.DELIMITER, line), lines)
  trimmed.lines <-
    Map(function(line) substr(line, nchar(LINE.DELIMITER) + 1, nchar(line)),
        delimited.lines)
  joined.lines <- do.call(paste, c(trimmed.lines, sep='\n'))
  if (is.nil(joined.lines))
    nil
  else {
###     print(joined.lines)
    elements <- car(strsplit(joined.lines, TAG.DELIMITER, fixed=T))
###     elements <- car(strsplit(joined.lines, TAG.DELIMITER, fixed=T))
###     print(str(elements))
    description <- car(elements)
    parsed.elements <- Reduce(function(parsed, element)
                              append(parsed, parse.element(element)),
                              cdr(elements),
                              if (is.null.string(description)) NULL
                              else parse.description(description))
  }
} 

parse.ref.srcref <- function(srcref) {
  srcfile <- attributes(srcref)$srcfile
  lines <- getSrcLines(srcfile, car(srcref), caddr(srcref))
  expression <- parse(text=lines)
  pivot <- caar(expression)
  parser <- parser.srcref(as.character(pivot))
  append(do.call(parser, list(expression)),
         list(srcref=list(filename=srcfile$filename,
                lloc=as.vector(srcref))))
         
}

parse.refs <- function(prerefs.srcrefs)
  Map(parse.ref, prerefs.srcrefs)

parse.file <- function(file) {
  srcfile <- srcfile(file)
  srcrefs <- attributes(parse(srcfile$filename,
                              srcfile=srcfile))$srcref
  parse.refs(zip.list(prerefs(srcfile, srcrefs), srcrefs))
}

parse.files <- function(...)
  Reduce(append, Map(parse.file, list(...)), NULL)
