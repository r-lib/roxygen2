#' @include functional.R
SPACE <- '[[:space:]]+'
MATTER <- '[^[:space:]]+'

trim.left <- function(string)
  gsub(sprintf('^%s', SPACE), '', string)

trim.right <- function(string)
  gsub(sprintf('%s$', SPACE), '', string)

trim <- function(string)
  Compose(trim.left, trim.right)(string)

is.null.string <- function(string) regexpr(MATTER, string) < 0

## Major source of inefficiency; resort to a words-string datatype
## with pre-delineated words?
words.default <- function(string, matter) gregexpr(matter, string)[[1]]

nwords.default <- function(string, words) length(words(string))

word.ref.default <- function(string, n, words) {
  words <- words(string)
  start <- words[[n]]
  length <- attributes(words)$match.length[[n]]
  end <- start + length - 1
  list(start=start, end=end)
}

strcar.default <- function(string, word.ref) {
  if (is.null.string(string))
    stop('CARing null-string')
  ref <- word.ref(string, 1)
  substr(string, ref$start, ref$end)
}

strcdr.default <- function(string, nwords, word.ref) {
  if (is.null.string(string))
    stop('CDRing null-string')
  nwords <- nwords(string)
  if (nwords == 1)
    ''
  else
    substr(string, word.ref(string, 2)$start, nchar(string))
}

words <- Curry(words.default, matter=MATTER)

nwords <- Curry(nwords.default, words=words)

word.ref <- Curry(word.ref.default, words=words)

strcar <- Curry(strcar.default, word.ref=word.ref)

strcdr <- Curry(strcdr.default, nwords=nwords, word.ref=word.ref)
