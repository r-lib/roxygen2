source('list.R')

LINE.DELIMITER <- '#\''
TAG.DELIMITER <- '@'

trim <- function(string)
  gsub('^[[:space:]]+', '',
       gsub('[[:space:]]+$', '', string))

#' Comment blocks (possibly null) that precede a file's expressions.
prerefs <- function(srcfile) {
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

parse.ref <- function(x, ...)
  UseMethod('parse.ref')

parse.ref.list <- function(preref.srcref)
  append(parse.ref(car(preref.srcref)),
         parse.ref(cadr(preref.srcref)))

parse.default <- function(...) {
  list(unknown=paste(...))
}

parse.description <- function(expression)
  list(description=expression)

parser <- function(key) {
  f <- sprintf('parse.%s', key)
  if (length(ls(pattern=f)) > 0) f else parse.default
}

paste.list <- function(list) {
  do.call(paste, list)
}

parse.element <- function(element) {
  tokens <- car(strsplit(element, ' ', fixed=T))
  parser <- parser(car(tokens))
  do.call(parser, as.list(cdr(tokens)))
}

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
  ## Forced to Reduce, since Map introduces magical name-mapping.
  parsed.elements <- Reduce(function(parsed, element)
                            append(parsed, parse.element(element)),
                            cdr(elements), parse.description(car(elements)))
} 

parse.ref.srcref <- function(srcref)
  nil

parse.refs <- function(prerefs.srcrefs)
  Map(parse.ref, prerefs.srcrefs)

srcfile <- srcfile('example.R')
srcrefs <- attributes(parse(srcfile$filename,
                            srcfile=srcfile))$srcref
## parse.refs(zip.list(prerefs(srcfile), srcrefs))
Map(unlist, Map(parse.ref, prerefs(srcfile)))
