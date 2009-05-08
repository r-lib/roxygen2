#' The empty list
nil <- list()

#' Whether a list is empty.
#' @param list the list to test
#' @return Whether the list is empty
is.nil <- function(list)
  length(list) == 0 || is.null(car(list))

#' First element of a list
#' @param list the list to first
#' @return The first element
car <- function(list)
  list[[1]]

#' Return elements after the first of a list.
#' @param list the list from which to extract
#' @return The elements after the first, or \code{nil}
#' if only one
cdr <- function(list) {
  if (is.nil(list))
    stop('CDRing a null list')
  length <- length(list)
  if (length == 1)
    nil
  else
    list[2:length]
}

#' Composite \code{car}/\code{cdr}
#' @param list the list from which to extract
#' @return The extracted elements
caar <- function(list) {
  car(car(list))
}

#' Composite \code{car}/\code{cdr}
#' @param list the list from which to extract
#' @return The extracted elements
cadr <- function(list) {
  car(cdr(list))
}

#' Composite \code{car}/\code{cdr}
#' @param list the list from which to extract
#' @return The extracted elements
cddr <- function(list) {
  cdr(cdr(list))
}

#' Composite \code{car}/\code{cdr}
#' @param list the list from which to extract
#' @return The extracted elements
caddr <- function(list) {
  car(cddr(list))
}

#' Composite \code{car}/\code{cdr}
#' @param list the list from which to extract
#' @return The extracted elements
cadar <- function(list) {
  cadr(car(list))
}

#' Composite \code{car}/\code{cdr}
#' @param list the list from which to extract
#' @return The extracted elements
cdddr <- function(list) {
  cddr(cdr(list))
}

#' Is a number even?
#' @param a the number to test
#' @return Whether the number is even
is.even <- function(a) {
  a %% 2 == 0
}

#' Is a number odd?
#' @param a the number to test
#' @return Whether the number is odd
is.odd <- function(a) {
  Negate(is.even)(a)
}

#' Zip \emph{n} lists together into tuplets of
#' length \emph{n}.
#' @param zipper the zipping function
#' @param \dots the lists to be zipped
#' @return A list of tuplets
zip <- function(zipper, ...) {
  m <- mapply(zipper, ...)
  split(m, col(m))
}

#' Zip using \code{c}.
#' @param \dots the lists to be zipped
#' @return A list of tuplets
#' @seealso \code{\link{zip}}
zip.c <- function(...) {
  zip(c, ...)
}

#' Zip using \code{list}.
#' @param \dots the lists to be zipped
#' @return A list of tuplets
#' @seealso \code{\link{zip}}
zip.list <- function(...) {
  zip(list, ...)
}

#' Combine a list into pairwise elements; lists should
#' be of the same length. In case of odd numbers of members,
#' the last will be removed.
#' @param list the list to be pairwise decomposed
#' @return A list of pairwise elements
pairwise <- function(list) {
  length <- length(list)
  if (length < 2)
    return(nil)
  length <- ifelse(is.odd(length),
                   length - 1,
                   length)
  odds <- seq(1, length, 2)
  evens <- seq(2, length, 2)
  zip.c(list[odds], list[evens])
}
