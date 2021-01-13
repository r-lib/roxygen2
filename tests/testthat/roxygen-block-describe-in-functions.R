#' Power
#' @param x base
#' @param exp exponent
power <- function(x, exp) x ^ exp

#' @describeIn power Square a number
square <- function(x) power(x, 2)

#' @describeIn power Cube a number
cube <- function(x) power(x, 3)
