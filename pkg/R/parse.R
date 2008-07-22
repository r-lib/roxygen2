#' @include string.R
#' @include list.R
#' @include functional.R
LINE.DELIMITER <- '#\''
TAG.DELIMITER <- '@'

#' No-op for sourceless files
#' @return \code{NULL}
roxygen <- function() NULL

#' Comment blocks (possibly null) that precede a file's expressions.
#' @param srcfile result of running \code{srcfile} on an interesting
#' file
#' @param srcrefs the resultant srcrefs
#' @return A list of prerefs that resemble srcrefs in form, i.e.
#' with srcfile and lloc
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

#' Preref parser table
preref.parsers <- new.env(parent=emptyenv())

#' Srcref parser table
srcref.parsers <- new.env(parent=emptyenv())

#' Register a parser with a table
#' @param table the table under which to register
#' @param key the key upon which to register
#' @param parser the parser callback to register;
#' a function taking \code{key} and \code{expression}
#' @return \code{NULL}
register.parser <- function(table, key, parser)
  table[[key]] <- parser

#' Specifically register a preref parser
#' @param key the key upon which to register
#' @param parser the parser callback to register;
#' a function taking \code{key} and \code{expression}
#' @return \code{NULL}
#' @seealso \code{\link{register.parser}}
register.preref.parser <- Curry(register.parser,
                                table=preref.parsers)

#' Specifically register a srcref parser
#' @param key the key upon which to register
#' @param parser the parser callback to register;
#' a function taking \code{key} and \code{expression}
#' @return \code{NULL}
#' @seealso \code{\link{register.parser}}
register.srcref.parser <- Curry(register.parser,
                                table=preref.parsers)

#' Register many parsers at once.
#' @param table the table under which to register
#' @param parser the parser to register
#' @param \dots the keys upon which to register
#' @return \code{NULL}
register.parsers <- function(table, parser, ...) {
  for (key in c(...))
    register.parser(table, key, parser)
}
  
#' Register many preref parsers at once.
#' @param parser the parser to register
#' @param \dots the keys upon which to register
#' @return \code{NULL}
register.preref.parsers <- Curry(register.parsers,
                                 table=preref.parsers)

#' Register many srcref parsers at once.
#' @param parser the parser to register
#' @param \dots the keys upon which to register
#' @return \code{NULL}
register.srcref.parsers <- Curry(register.parsers,
                                 table=srcref.parsers)

#' Centrally formatted message
#' @param key the offending key
#' @param message the apposite message
#' @return The formatted message
parse.message <- function(key, message)
  sprintf('@%s %s.', key, message)

#' Centrally formatted error; stopping execution
#' @param key the offending key
#' @param message the apposite message
#' @return \code{NULL}
parse.error <- function(key, message)
  stop(parse.message(key, message))

#' Centrally formatted warning
#' @param key the offending key
#' @param message the apposite message
#' @return \code{NULL}
parse.warning <- function(key, message)
  warning(parse.message(key, message))

#' Parse a raw string containing key and expressions.
#' @param element the string containing key and expressions
#' @return A list containing the parsed constituents
parse.element <- function(element) {
  tag <- strcar(element)
  rest <- strcdr(element)
  parser <- parser.preref(tag)
  do.call(parser, list(tag, rest))
}

#' Parse description: the premier part of a roxygen block
#' containing description and option details separated by
#' a blank roxygen line.
#' @param expression the description to be parsed
#' @return A list containing the parsed description
parse.description <- function(expression)
  list(description=expression)

#' Default parser which simply emits the key and expression.
#' @param key the parsing key
#' @param rest the expression to be parsed
#' @return A list containing the key and expression (possibly
#' null)
parse.default <- function(key, rest)
  as.list(structure(rest, names=key))

#' Resorts to the default parser but with a warning about the
#' unknown key.
#' @param key the parsing key
#' @param rest the expression to be parsed
#' @return A list containing the key and expression (possibly
#' null)
#' @seealso \code{\link{parse.default}}
parse.preref <- function(key, rest) {
  parse.warning(key, 'is an unknown key')
  parse.default(key, rest)
}

#' Possibly NA; in which case, the Roclets can do something more
#' sophisticated with the srcref.
register.preref.parser('export', parse.default)

#' Parse an element with a mandatory value.
#' @param key the parsing key
#' @param rest the expression to be parsed
#' @return A list containing the key and value
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

#' Parse an element containing a mandatory name
#' and description (such as @@param).
#' @param key the parsing key
#' @param rest the expression to be parsed
#' @return A list containing the key, name and
#' description
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

#' Parse an element containing a single name and only a name;
#' extra material will be ignored and a warning issued.
#' @param key parsing key
#' @param name the name to be parsed
#' @return A list containing key and name
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

#' Turn a binary element on; parameters are ignored.
#' @param key parsing key
#' @param rest the expression to be parsed
#' @return A list with the key and \code{TRUE}
parse.toggle <- function(key, rest)
  as.list(structure(TRUE, names=key))

register.preref.parsers(parse.toggle,
                        'listObject',
                        'attributeObject',
                        'environmentObject')

#' By default, srcrefs are ignored; this parser returns \code{nil}.
#' @param pivot the parsing pivot
#' @param expression the expression to be parsed
#' @return \code{nil}
parse.srcref <- function(pivot, expression) nil

#' Parse S4 \code{setClass} method.
#' @param pivot the parsing pivot
#' @param expression the expression to be parsed
#' @return An list containing the class to be set
register.srcref.parser('setClass',
                       function(pivot, expression)
                       list(class=cadr(car(expression))))

#' Parse S4 \code{setGeneric} method.
#' @param pivot the parsing pivot
#' @param expression the expression to be parsed
#' @return A list containing the generic
register.srcref.parser('setGeneric',
                       function(pivot, expression)
                       list(generic=cadr(car(expression))))

#' Parse S4 \code{setMethod} method.
#' @param pivot the parsing pivot
#' @param expression the expression to be parsed
#' @return A list containing the method to be set
register.srcref.parser('setMethod',
                       function(pivot, expression)
                       list(method=cadr(car(expression)),
                            signature=caddr(car(expression))))

#' Default parser-lookup; if key not found, return
#' the default parser specified.
#' @param table the parser table from which to look
#' @param key the key upon which to look
#' @param default the parser to return upon unsuccessful
#' lookup
#' @return The parser
parser.default <- function(table, key, default)
  if (is.null(f <- table[[key]])) default else f

#' Preref parser-lookup; defaults to \code{parse.preref}.
#' @param key the key upon which to look
#' @return The parser
parser.preref <- Curry(parser.default,
                       table=preref.parsers,
                       default=parse.preref)

#' Srcref parser-lookup; defaults to \code{parse.srcref}.
#' @param key the key upon which to look
#' @return The parser
parser.srcref <- Curry(parser.default,
                       table=srcref.parsers,
                       default=parse.srcref)

#' Parse either srcrefs, prerefs or pairs of the same.
#' @param ref the srcref, preref or pair of the same
#' @return List containing the parsed srcref/preref
parse.ref <- function(ref, ...)
  UseMethod('parse.ref')

#' Parse a preref/srcrefs pair
#' @param ref the preref/srcref pair
#' @return List combining the parsed preref/srcref
parse.ref.list <- function(ref, ...)
  append(parse.ref(car(ref)),
         parse.ref(cadr(ref)))


#' Parse a preref
#' @param ref the preref to be parsed
#' @return List containing the parsed preref
parse.ref.preref <- function(ref, ...) {
  lines <- getSrcLines(attributes(ref)$srcfile,
                       car(ref),
                       caddr(ref))
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
                             perl=TRUE))
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

#' Parse a srcref
#' @param ref the srcref to be parsed
#' @return List containing the parsed srcref
parse.ref.srcref <- function(ref, ...) {
  srcfile <- attributes(ref)$srcfile
  lines <- getSrcLines(srcfile, car(ref), caddr(ref))
  expression <- parse(text=lines)
  pivot <- tryCatch(caar(expression), error=function(e) NULL)
  parsed <- list(srcref=list(filename=srcfile$filename,
                   lloc=as.vector(ref)))
  if (!is.null(pivot)) {
    parser <- parser.srcref(as.character(pivot))
    parsed <- append(do.call(parser, list(pivot, expression)),
                     parsed)
  }
  parsed
}

#' Parse each of a list of preref/srcref pairs.
#' @param preref.srcrefs list of preref/srcref pairs
#' @return List combining parsed preref/srcrefs
parse.refs <- function(preref.srcrefs)
  Map(parse.ref, preref.srcrefs)

#' Parse a source file containing roxygen directives.
#' @param file string naming file to be parsed
#' @return List containing parsed directives
parse.file <- function(file) {
  srcfile <- srcfile(file)
  srcrefs <- attributes(parse(srcfile$filename,
                              srcfile=srcfile))$srcref
  if (length(srcrefs) > 0)
    parse.refs(zip.list(prerefs(srcfile, srcrefs), srcrefs))
  else
    nil
}

#' Parse many files at one.
#' @param \dots files to be parsed
#' @return List containing parsed directives
#' @seealso \code{\link{parse.file}}
parse.files <- function(...)
  Reduce(append, Map(parse.file, list(...)), NULL)
