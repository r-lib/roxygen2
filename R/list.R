nil <- list()

is.nil <- function(list)
  length(list) == 0 || is.null(car(list))

car <- function(list) {
  list[[1]]
}

cdr <- function(list) {
  list[2:length(list)]
}

cadr <- function(list) {
  car(cdr(list))
}

cddr <- function(list) {
  cdr(cdr(list))
}

caddr <- function(list) {
  car(cddr(list))
}

is.even <- function(a) {
  a %% 2 == 0
}

is.odd <- function(a) {
  Negate(is.even)(a)
}

zip <- function(zipper, ...) {
  m <- mapply(zipper, ...)
  split(m, col(m))
}


zip.c <- function(...) {
  zip(c, ...)
}

zip.list <- function(...) {
  zip(list, ...)
}

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
