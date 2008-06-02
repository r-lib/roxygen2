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

caddr <- function(list) {
  car(cdr(cdr(list)))
}

is.even <- function(a) {
  a %% 2 == 0
}

is.odd <- function(a) {
  Negate(is.even)(a)
}

zip <- function(...) {
  m <- mapply(c, ...)
  split(m, col(m))
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
  zip(list[odds], list[evens])
}
