#' Power
#' @param x base
#' @param exp exponent
power <- function(x, exp) x ^ exp
# this is destination

#' @describeIn power Square a number
square <- function(x) power(x, 2)
# this is a source function

#' @describeIn power Cube a number
cube <- function(x) power(x, 3)
# this is another source function
