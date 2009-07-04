#' @include roxygen.R
#' @include functional.R
#' @include string.R
#' @include list.R
roxygen()

#' Sequence that distinguishes roxygen comment from normal comment.
LINE.DELIMITER <- '#+\''

#' Symbol that delimits tags.
TAG.DELIMITER <- '@'

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
#' @TODO number parser?
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
#' @export
register.preref.parser <- Curry(register.parser,
                                table=preref.parsers)

#' Specifically register a srcref parser
#' @param key the key upon which to register
#' @param parser the parser callback to register;
#' a function taking \code{key} and \code{expression}
#' @return \code{NULL}
#' @seealso \code{\link{register.parser}}
#' @export
register.srcref.parser <- Curry(register.parser,
                                table=srcref.parsers)

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
#' @export
register.preref.parsers <- Curry(register.parsers,
                                 table=preref.parsers)

#' Register many srcref parsers at once.
#' @param parser the parser to register
#' @param \dots the keys upon which to register
#' @return \code{NULL}
#' @export
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
  warning(parse.message(key, message), immediate.=TRUE)

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

#' Default parser which simply emits the key and expression;
#' used for elements with optional values (like \code{@@export})
#' where roclets can do more sophisticated things with \code{NULL}.
#' @param key the parsing key
#' @param rest the expression to be parsed
#' @return A list containing the key and expression (possibly
#' null)
#' @export
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

#' Parse an element with a mandatory value.
#' @param key the parsing key
#' @param rest the expression to be parsed
#' @return A list containing the key and value
#' @export
parse.value <- function(key, rest) {
  if (is.null.string(rest))
    parse.error(key, 'requires a value')
  else
    parse.default(key, rest)
}
  
#' Parse an element containing a mandatory name
#' and description (such as \code{@@param}).
#' @param key the parsing key
#' @param rest the expression to be parsed
#' @return A list containing the key, name and
#' description
#' @export
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

#' Parse an element containing a single name and only a name;
#' extra material will be ignored and a warning issued.
#' @param key parsing key
#' @param name the name to be parsed
#' @return A list containing key and name
#' @export
parse.name <- function(key, name) {
  if (is.null.string(name))
    parse.error(key, 'requires a name')
  else if (nwords(name) > 1)
    parse.warning(key, 'ignoring extra arguments')
  parse.default(key, strcar(name))
}

#' Turn a binary element on; parameters are ignored.
#' @param key parsing key
#' @param rest the expression to be parsed
#' @return A list with the key and \code{TRUE}
#' @export
parse.toggle <- function(key, rest)
  as.list(structure(TRUE, names=key))

#' By default, srcrefs are ignored; this parser returns \code{nil}.
#' @param pivot the parsing pivot
#' @param expression the expression to be parsed
#' @return \code{nil}
parse.srcref <- function(pivot, expression) nil

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
#' @param \dots ignored
#' @return List containing the parsed srcref/preref
#' @export
parse.ref <- function(ref, ...)
  UseMethod('parse.ref')

#' Parse a preref/srcrefs pair
#' @method parse.ref list
#' @param ref the preref/srcref pair
#' @param \dots ignored
#' @return List combining the parsed preref/srcref
#' @export
parse.ref.list <- function(ref, ...)
  append(parse.ref(car(ref)),
         parse.ref(cadr(ref)))


#' Parse a preref
#' @method parse.ref preref
#' @param ref the preref to be parsed
#' @param \dots ignored
#' @return List containing the parsed preref
#' @export
parse.ref.preref <- function(ref, ...) {
  lines <- Map(trim.left, getSrcLines(attributes(ref)$srcfile,
                                      car(ref),
                                      caddr(ref)))
  delimited.lines <-
    Filter(function(line) grep(LINE.DELIMITER, line), lines)
  ## Take next word after delimiter.
  trimmed.lines <- Map(strcdr, delimited.lines)
  joined.lines <- do.call(paste, c(trimmed.lines, sep='\n'))
  if (is.nil(joined.lines))
    nil
  else {
    ## Thanks to Fegis at #regex on Freenode for the
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

#' Recursively walk an expression (as returned by \code{parse}) in
#' preorder.
#' @param proc the procedure to apply to each subexpression
#' @param expression the root of the expression
#' @return NULL
#' @export
preorder.walk.expression <- function(proc, expression) {
  if (length(expression) > 0)
    for (i in c(1:length(expression))) {
      member <- tryCatch(expression[[i]], error=function(e) NULL)
      if (!is.null(member) && !identical(member, expression)) {
        proc(member)
        try(preorder.walk.expression(proc, member),
            silent=TRUE)
      }
    }
}

#' Flatten a nested expression into a list, preorderly.
#' @param expression the root of the expression to be
#' flattened
#' @return A list containing the flattened expression
#' @export
preorder.flatten.expression <- function(expression) {
  flattened <- NULL
  preorder.walk.expression(function(expression)
      flattened <<- append(flattened, expression),
      expression)
  flattened
}

#' Whether the expression implies assignment by \code{<-}
#' or \code{=}.
#' @param expression the expression to check for assignment
#' @return Whether or not the expression assigns by \code{<-}
#' \code{=}
is.assignment <- function(expression) {
  class <- class(expression)
  class == '<-' | class == '='
}

#' Whether the expression assigns function
#' @param expression the expression to check for assignment
#' @return Whether the expression assigns a function
is.function.definition <- function(expression)
  expression == 'function'

#' Find the formal arguments associated with a given
#' expression (may be \code{NULL}).
#' @param expressions the expressions from which to extract
#' formal arguments
#' @return The formal arguments of said expression or
#' \code{NULL}
parse.formals <- function(expressions) {
  formals <- NULL
  call <- car(expressions)
  if (is.call(call)) {
    f <- cadr(expressions)
    if (is.function.definition(f))
      formals <- tryCatch(formals(eval(call)),
                          error=function(e) NULL)
  }
  if (is.null(formals)) formals
  else list(formals=Map(function(formal)
              if (is.null(formal)) ''
              else if (is.call(formal)) capture.output(formal)
              else as.character(maybe.quote(formal)), formals))
}

#' Find the assignee of the expression
#' @param expression the expression in which to find the
#' assignee
#' @return The expression's assignee
parse.assignee <- function(expression)
  list(assignee=as.character(car(expression)))

#' Parse a function call, paying special attention to
#' assignments by \code{<-} or \code{=}.
#' @param expressions the expression to search through
#' @return List of formals and assignee in case of
#' assignment, the processed expression in case of
#' non-assigning function calls (see \code{parse.srcref}).
parse.call <- function(expressions) {
  call <- car(expressions)
  if (is.assignment(call)) {
    assignee <- parse.assignee(cddr(expressions))
    formals <- parse.formals(cdddr(expressions))
    append(assignee, formals)
  } else {
    lhs <- cadr(as.character(expressions))
    parser.srcref(lhs)(lhs, cddr(expressions))
  }
}

#' Parse a srcref
#' @method parse.ref srcref
#' @param ref the srcref to be parsed
#' @param \dots ignored
#' @return List containing the parsed srcref
#' @export
parse.ref.srcref <- function(ref, ...) {
  srcfile <- attributes(ref)$srcfile
  srcref <- list(srcref=list(filename=srcfile$filename,
                   lloc=as.vector(ref)))
  lines <- getSrcLines(srcfile, car(ref), caddr(ref))
  expressions <- preorder.flatten.expression(parse(text=lines))
  parsed <- NULL
  if (is.call(car(expressions)))
    parsed <- parse.call(expressions)
  append(parsed, srcref)
}

#' Parse each of a list of preref/srcref pairs.
#' @param preref.srcrefs list of preref/srcref pairs
#' @return List combining parsed preref/srcrefs
parse.refs <- function(preref.srcrefs)
  Map(parse.ref, preref.srcrefs)

#' Parse a source file containing roxygen directives.
#' @param file string naming file to be parsed
#' @return List containing parsed directives
#' @export
#' @callGraph
#' @callGraphDepth 3
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
#' @export
parse.files <- function(...)
  Reduce(append, Map(parse.file, list(...)), NULL)

#' Text-parsing hack using tempfiles for more facility.
#' @param \dots lines of text to be parsed
#' @return The parse tree
#' @export
parse.text <- function(...) {
  file <- tempfile()
  cat(..., sep='\n', file=file)
  parse.file(file)
}
