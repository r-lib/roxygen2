source("list.R")
source("curry.R")
filter.artifacts <- function(pair)
  car(pair) <= cdr(pair)
length.line <- function(srcfile, lineno)
  nchar(getSrcLines(srcfile, lineno, lineno))
pair.srcref <- function(file, pair) {
  srcfile <- srcfile(file)
  start <- car(pair)
  end <- cdr(pair)
  srcref(srcfile, c(start, 1, end, length.line(srcfile, end)))
}
#' Comment blocks (possibly null) that precede a file's expressions.
prerefs <- function(file) {
  pairs <-
    pairwise(c(1, unlist(Map(function(srcref)
                             c(car(srcref) - 1,
                               caddr(srcref) + 1),
                             attributes(parse(file))$srcref))))
  Map(Curry(pair.srcref, file=file),
      Filter(filter.artifacts, pairs))
}
prerefs('example.R')
