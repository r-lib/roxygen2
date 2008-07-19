nil <- list()

is.nil <- function(list)
  length(list) == 0 || is.null(car(list))

car <- function(list) {
  list[[1]]
}

cdr <- function(list) {
  length <- length(list)
  cdr <- switch(length + 1,
                stop('CDRing a null list'),
                nil)
  if(is.null(cdr))
    list[2:length]
  else
    cdr
}

caar <- function(list) {
  car(car(list))
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

cadar <- function(list) {
  cadr(car(list))
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
