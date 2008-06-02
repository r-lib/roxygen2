source('list.R')
source('curry.R')

LINE.DELIMITER <- '#\''
TAG.DELIMITER <- '@'

trim <- function(x, ...)
  UseMethod('trim')

trim.character <- function(string)
  gsub('^[[:space:]]+', '',
       gsub('[[:space:]]+$', '', string))

incipit <- function(x, ...)
  UseMethod('incipit')
incipit.preref <- incipit.srcref <- car

terminus <- function(x, ...)
  UseMethod('terminus')
terminus.preref <- terminus.srcref <- caddr

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
                      c(incipit(srcref) - 1,
                        terminus(srcref) + 1),
                      srcrefs))
  pairs <- pairwise(c(1, lines))
  Map(pair.preref, pairs)
}

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
  joined.lines <- gsub(' {2,}', ' ', do.call(paste, trimmed.lines))
  elements <- Map(trim, strsplit(joined.lines, TAG.DELIMITER))
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
