source("list.R")
filter.artifacts <- function(pair)
  car(pair) <= cdr(pair)

#' Comment blocks (possibly null) that precede a file's expressions.
prerefs <- function(srcfile) {
  length.line <- function(lineno)
    nchar(getSrcLines(srcfile, lineno, lineno))

  pair.srcref <- function(pair) {
    start <- car(pair)
    end <- cdr(pair)
    srcref(srcfile, c(start, 1, end, length.line(end)))
  }

  lines <- unlist(Map(function(srcref)
                      c(car(srcref) - 1, caddr(srcref) + 1),
                      srcrefs))
  pairs <- pairwise(c(1, lines))
  Map(pair.srcref, Filter(filter.artifacts, pairs))
}
srcfile <- srcfile('example.R')
srcrefs <- attributes(parse(srcfile$filename,
                            srcfile=srcfile))$srcref
prerefs(srcfile)
