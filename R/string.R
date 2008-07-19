#' @include functional.R @include list.R
SPACE <- '[[:space:]]+'
MATTER <- '[^[:space:]]+'
NIL.STRING <- ''

trim.left <- function(string)
  gsub(sprintf('^%s', SPACE), NIL.STRING, string)

trim.right <- function(string)
  gsub(sprintf('%s$', SPACE), NIL.STRING, string)

trim <- function(string)
  Compose(trim.left, trim.right)(string)

is.null.string <- function(string) regexpr(MATTER, string) < 0

word.ref <- function(string, n) {
  continue <- function(string, n, init) {
    word <- regexpr(MATTER, string)
    start <- word[[1]]
    length <- attributes(word)$match.length[[1]]
    end <- start + length
    if (n <= 1) list(start=start + init, end=end + init)
    else continue(substr(string, end, nchar(string)), n - 1, init + length)
  }
  continue(string, n, 0)
}

strcar <- function(string) {
  if (is.null.string(string))
    stop('CARing null-string')
  ref <- word.ref(string, 1)
  substr(string, ref$start, ref$end - 1)
}

strcdr <- function(string) {
  if (is.null.string(string))
    stop('CDRing null-string')
  ref <- word.ref(string, 2)
  if (ref$end < ref$start)
    NIL.STRING
  else
    substr(string, ref$start, nchar(string))
}

strcons <- function(consor, consee, sep) {
  if (is.null.string(consee)) consor
  else paste(consor, consee, sep=sep)
}

## General enough to be designated `map': isn't it closer to a
## specialized reduce?
strmap <- function(proc, sep, string) {
  continue <- function(string)
    if (is.null.string(string))
      NIL.STRING
    else
      strcons(strcar(string),
              continue(strcdr(string)),
              sep=sep)
  continue(string)
}

debug <- function(...) {
  values <- list(...)
  var.values <- zip.list(attributes(values)$names, values)
  cat(Reduce.paste(function(var.value)
                   sprintf('%s: %s; ', car(var.value), cadr(var.value)),
                   var.values,
                   NIL.STRING),
      '\n')
}

Reduce.paste <- function(proc, elts, sep)
  Reduce(function(parsed, elt)
         Curry(paste, sep=sep)
         (parsed, proc(elt)),
         elts,
         NIL.STRING)
