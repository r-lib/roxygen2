#' @include roxygen.R
#' @include functional.R
#' @include list.R
roxygen()

#' Absence of words
SPACE <- '[[:space:]]+'

#' Anti-anti-words
MATTER <- '[^[:space:]]+'

#' Analogue to the empty list
NIL.STRING <- ''

#' Trim [:space:] to the left of a string.
#' @param string the string to be trimmed
#' @return A left-trimmed string
trim.left <- function(string)
  gsub(sprintf('^%s', SPACE), NIL.STRING, string)

#' Trim [:space:] to the right of a string.
#' @param string the string to be trimmed
#' @return A right-trimmed string
trim.right <- function(string)
  gsub(sprintf('%s$', SPACE), NIL.STRING, string)

#' Trim [:space:] on both sides of a string.
#' @param string the string to be trimmed
#' @return A trimmed string
trim <- function(string)
  Compose(trim.left, trim.right)(string)

#' Does the string contain no matter, but very well [:space:]?
#' @param string the string to check
#' @return TRUE if the string contains words, otherwise FALSE
is.null.string <- function(string) {
  if (is.na(string)) FALSE
  else regexpr(MATTER, string) < 0
}

#' Number of words a string contains.
#' @param string the string whose words to count
#' @return Number of words in the string
nwords <- function(string) {
  if (is.null.string(string)) 0
  else length(gregexpr(MATTER, string))
}

#' Find the nth word in a string.
#' @param string the string to search in
#' @param n the nth word to find
#'
#' @return A list containing:
#'   \item{start}{the first letter of the word.}
#'   \item{end}{the last letter of the word.}
#' Undefined if no such word; though \code{end} may be less than
#' \code{start} in such a case.
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

#' First word in a string.
#' @param string the string whose word to finde
#' @return The first word
strcar <- function(string) {
  if (is.null.string(string))
    stop('CARing null-string')
  ref <- word.ref(string, 1)
  substr(string, ref$start, ref$end - 1)
}

#' Words after first in a string.
#' @param string the string whose words to find
#' @return The words after first in the string
strcdr <- function(string) {
  if (is.null.string(string))
    stop('CDRing null-string')
  ref <- word.ref(string, 2)
  if (ref$end < ref$start)
    NIL.STRING
  else
    substr(string, ref$start, nchar(string))
}

#' Join two string.
#' @param consor the joining string
#' @param consee the joined string
#' @param sep the intervening space
#' @return The joined strings
strcons <- function(consor, consee, sep) {
  if (is.null.string(consee)) consor
  else paste(consor, consee, sep=sep)
}

#' Map through the words in a string, joining the mapped
#' words with a separator.
#'
#' General enough to be designated `map': isn't it closer to a
#' specialized reduce?
#' @param proc procedure to apply to each word
#' @param sep the separator joining the mapped words
#' @param string the string to be mapped
#' @return Mapped words separated by \code{sep}
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

#' Convenience function to print variable-value pairs.
#'
#' @param \dots named variable of the form a=b, \dots
#' @return NULL
debug <- function(...) {
  values <- list(...)
  var.values <- zip.list(attributes(values)$names, values)
  cat(Reduce.paste(function(var.value)
                   sprintf('%s: %s; ', car(var.value), cadr(var.value)),
                   var.values,
                   NIL.STRING),
      '\n')
}

#' Ad-hoc abstraction to paste processed list-elements together.
#' @param proc the procedure to apply to the elements
#' @param elts the elements to be processed
#' @param sep the glue to joined the processed elements
#' @return The processed elements as a glued string
Reduce.paste <- function(proc, elts, sep)
  Reduce(function(parsed, elt)
         Curry(paste, sep=sep)
         (parsed, proc(elt)),
         elts,
         NIL.STRING)

#' Actually do the substring representation that
#' regexpr should do; does not acknowledge groups,
#' since \code{regexpr} doesn't.
#' @param pattern the pattern to match
#' @param text the text to match against
#' @return The matched substring
substr.regexpr <- function(pattern, text) {
  matches <- regexpr(pattern, text, perl=TRUE)
  if (length(match) < 1)
    NULL
  else {
    start <- car(matches)
    end <- car(attr(matches, 'match.length'))
    substr(text, start, end)
  }
}

#' Quote characters and return all other types untouched.
#' @param x the value to maybe quote
#' @return The quoted character or the untouched value
maybe.quote <- function(x) {
  if (is.character(x))
    sprintf('"%s"', x)
  else
    x
}
