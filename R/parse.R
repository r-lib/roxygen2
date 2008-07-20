#' @include string.R
#' @include list.R
LINE.DELIMITER <- '#\''
TAG.DELIMITER <- '@'

#' No-op for sourceless files
roxygen <- function() NULL

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

preref.parsers <- new.env(parent=emptyenv())

srcref.parsers <- new.env(parent=emptyenv())

register.parser <- function(table, key, parser)
  table[[key]] <- parser

register.preref.parser <- Curry(register.parser,
                                table=preref.parsers)

register.srcref.parser <- Curry(register.parser,
                                table=preref.parsers)

register.parsers <- function(table, parser, ...) {
  for (key in c(...))
    register.parser(table, key, parser)
}
  
register.preref.parsers <- Curry(register.parsers,
                                 table=preref.parsers)

register.srcref.parsers <- Curry(register.parsers,
                                 table=srcref.parsers)

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
  do.call(parser, list(tag, rest))
}

## preref parsers

parse.description <- function(expression)
  list(description=expression)

parse.default <- function(key, rest)
  as.list(structure(rest, names=key))

parse.preref <- function(key, rest) {
  parse.warning(key, 'is an unknown key')
  parse.default(key, rest)
}

## Possibly NA; in which case, the Roclets can do something more
## sophisticated with the srcref.
register.preref.parser('export', parse.default)

parse.value <- function(key, rest) {
  if (is.null.string(rest))
    parse.error(key, 'requires a value')
  else
    parse.default(key, rest)
}
  
register.preref.parsers(parse.value,
                        'prototype',
                        'exportClass',
                        'exportMethod',
                        'exportPattern',
                        'S3method',
                        'import',
                        'importFrom',
                        'importClassesFrom',
                        'importMethodsFrom',
                        'name',
                        'aliases',
                        'title',
                        'usage',
                        'references',
                        'concept',
                        'note',
                        'seealso',
                        'examples',
                        'keywords',
                        'return',
                        'author',
                        'include')

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

register.preref.parsers(parse.name.description,
                        'slot',
                        'param')

parse.name <- function(key, name) {
  if (is.null.string(name))
    parse.error(key, 'requires a name')
  else if (nwords(name) > 1)
    parse.warning(key, 'ignoring extra arguments')
  parse.default(key, strcar(name))
}

register.preref.parsers(parse.name,
                        'S3class',
                        'returnType')

parse.toggle <- function(key, rest)
  as.list(structure(T, names=key))

register.preref.parsers(parse.toggle,
                        'listObject',
                        'attributeObject',
                        'environmentObject')

## srcref parsers

parse.srcref <- function(pivot, expression) nil

register.srcref.parser('setClass',
                       function(pivot, expression)
                       list(class=cadr(car(expression))))

register.srcref.parser('setGeneric',
                       function(pivot, expression)
                       list(generic=cadr(car(expression))))
register.srcref.parser('setMethod',
                       function(pivot, expression)
                       list(method=cadr(car(expression)),
                            signature=caddr(car(expression))))

## Parser lookup

parser.default <- function(table, key, default)
  if (is.null(f <- table[[key]])) default else f

parser.preref <- Curry(parser.default,
                       table=preref.parsers,
                       default=parse.preref)

parser.srcref <- Curry(parser.default,
                       table=srcref.parsers,
                       default=parse.srcref)

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
  ## Trim LINE.DELIMITER + one space (benign for spaceless delimeters).
  trimmed.lines <-
    Map(function(line) substr(line, nchar(LINE.DELIMITER) + 2, nchar(line)),
        delimited.lines)
  joined.lines <- do.call(paste, c(trimmed.lines, sep='\n'))
  if (is.nil(joined.lines))
    nil
  else {
    ## Thanks to Fegis on #regex at Freenode for the
    ## lookahead/lookbehind hack; as he notes, however, "it's not
    ## proper escaping though... it will not split a@@@b."
    elements <- car(strsplit(joined.lines,
                             sprintf('(?<!%s)%s(?!%s)',
                                     TAG.DELIMITER,
                                     TAG.DELIMITER,
                                     TAG.DELIMITER),
                             perl=T))
    ## Compress the escaped delimeters.
    elements <- Map(function(element)
                    gsub(sprintf('%s{2}', TAG.DELIMITER),
                         TAG.DELIMITER,
                         element),
                    elements)
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
  pivot <- tryCatch(caar(expression), error=function(e) NULL)
  parsed <- list(srcref=list(filename=srcfile$filename,
                   lloc=as.vector(srcref)))
  if (!is.null(pivot)) {
    parser <- parser.srcref(as.character(pivot))
    parsed <- append(do.call(parser, list(pivot, expression)),
                     parsed)
  }
  parsed
}

parse.refs <- function(preref.srcrefs)
  Map(parse.ref, preref.srcrefs)

parse.file <- function(file) {
  srcfile <- srcfile(file)
  srcrefs <- attributes(parse(srcfile$filename,
                              srcfile=srcfile))$srcref
  if (length(srcrefs) > 0)
    parse.refs(zip.list(prerefs(srcfile, srcrefs), srcrefs))
  else
    nil
}

parse.files <- function(...)
  Reduce(append, Map(parse.file, list(...)), NULL)
