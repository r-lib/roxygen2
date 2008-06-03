source('list.R')
source('functional.R')

LINE.DELIMITER <- '#\''
TAG.DELIMITER <- '@'

trim <- function(string)
  gsub('^[[:space:]]+', '',
       gsub('[[:space:]]+$', '', string))

paste.list <- function(list) {
  do.call(paste, list)
}

## Comment blocks (possibly null) that precede a file's expressions.
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

parse.preref <- function(...) {
  list(unknown=paste(...))
}

parse.element <- function(element) {
  tokens <- car(strsplit(element, ' ', fixed=T))
  parser <- parser.preref(car(tokens))
  do.call(parser, as.list(cdr(tokens)))
}

parse.description <- function(expression)
  list(description=expression)

parse.prototype <- function(...)
  list(prototype=paste(...))

parse.export <- function(...)
  list(export=T)

parse.name.description <- function(name, ...)
  list(slot=list(name=name, description=paste(...)))

parse.slot <- parse.name.description

parse.param <- parse.name.description

## srcref parsers

parse.srcref <- function(...) nil

parse.setClass <- function(expression)
  list(class=cadr(car(expression)))

parse.setGeneric <- function(expression)
  list(method=cadr(car(expression)))

parse.setMethod <- function(expression)
  list(method=cadr(car(expression)),
       class=caddr(car(expression)))

## Parser lookup

parser.default <- function(key, default) {
  f <- sprintf('parse.%s', key)
  if (length(ls(1, pattern=f)) > 0) f else default
}

parser.preref <- Curry(parser.default, default=parse.preref)

parser.srcref <- Curry(parser.default, default=parse.srcref)

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
  ## Presumption: white-space is insignificant; there are no
  ## multi-line elements. This contradicts, for instance, verbatim or
  ## latex.
  joined.lines <- gsub(' {2,}', ' ', paste.list(trimmed.lines))
  elements <- Map(trim, car(strsplit(joined.lines, TAG.DELIMITER, fixed=T)))
  parsed.elements <- Reduce(function(parsed, element)
                            append(parsed, parse.element(element)),
                            cdr(elements), parse.description(car(elements)))
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
