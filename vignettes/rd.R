## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)

## ------------------------------------------------------------------------
#' Add together two numbers
#'
#' @param x A number
#' @param y A number
#' @return The sum of \code{x} and \code{y}
#' @examples
#' add(1, 1)
#' add(10, 1)
add <- function(x, y) {
  x + y
}

## ------------------------------------------------------------------------
#' Sum of vector elements.
#'
#' \code{sum} returns the sum of all the values present in its arguments.
#'
#' This is a generic function: methods can be defined for it directly
#' or via the \code{\link{Summary}} group generic. For this to work properly,
#' the arguments \code{...} should be unnamed, and dispatch is on the
#' first argument.
sum <- function(..., na.rm = TRUE) {}

## ------------------------------------------------------------------------
#' @title Sum of vector elements.
#'
#' @description
#' \code{sum} returns the sum of all the values present in its arguments.
#'
#' @details
#' This is a generic function: methods can be defined for it directly
#' or via the \code{\link{Summary}} group generic. For this to work properly,
#' the arguments \code{...} should be unnamed, and dispatch is on the
#' first argument.
sum <- function(..., na.rm = TRUE) {}

## ------------------------------------------------------------------------
#' @family aggregate functions
#' @seealso \code{\link{prod}} for products, \code{\link{cumsum}} for
#'  cumulative sums, and \code{\link{colSums}}/\code{\link{rowSums}}
#'  marginal sums over high-dimensional arrays.

## ------------------------------------------------------------------------
#' Sum of vector elements.
#'
#' \code{sum} returns the sum of all the values present in its arguments.
#'
#' This is a generic function: methods can be defined for it directly
#' or via the \code{\link{Summary}} group generic. For this to work properly,
#' the arguments \code{...} should be unnamed, and dispatch is on the
#' first argument.
#'
#' @param ... Numeric, complex, or logical vectors.
#' @param na.rm A logical scalar. Should missing values (including NaN)
#'   be removed?
#' @return If all inputs are integer and logical, then the output
#'   will be an integer. If integer overflow
#'   \url{http://en.wikipedia.org/wiki/Integer_overflow} occurs, the output
#'   will be NA with a warning. Otherwise it will be a length-one numeric or
#'   complex vector.
#'
#'   Zero-length vectors have sum 0 by definition. See
#'   \url{http://en.wikipedia.org/wiki/Empty_sum} for more details.
#' @examples
#' sum(1:10)
#' sum(1:5, 6:10)
#' sum(F, F, F, T, T)
#'
#' sum(.Machine$integer.max, 1L)
#' sum(.Machine$integer.max, 1)
#'
#' \dontrun{
#' sum("a")
#' }
sum <- function(..., na.rm = TRUE) {}

## ------------------------------------------------------------------------
#' An S4 class to represent a bank account.
#'
#' @slot balance A length-one numeric vector
Account <- setClass("Account",
  slots = list(balance = "numeric")
)

## ------------------------------------------------------------------------
#' A Reference Class to represent a bank account.
#'
#' @field balance A length-one numeric vector
Account <- setRefClass("Account",
  fields = list(balance = "numeric"),
  methods = list(
    withdraw = function(x) {
      "Withdraw money from account. Allows overdrafts"
      balance <<- balance - x
    }
  )
)

## ------------------------------------------------------------------------
#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds. The variables are as follows:
#'
#' \itemize{
#'   \item price. price in US dollars (\$326--\$18,823)
#'   \item carat. weight of the diamond (0.2--5.01)
#'   ...
#' }
#'
#' @format A data frame with 53940 rows and 10 variables
#' @source \url{http://www.diamondse.info/}
"diamonds"

## ------------------------------------------------------------------------
#' Generate R documentation from inline comments.
#'
#' Roxygen2 allows you to write documentation in comment blocks co-located
#' with code.
#'
#' The only function you're likely to need from \pkg{roxygen2} is
#' \code{\link{roxygenize}}. Otherwise refer to the vignettes to see
#' how to format the documentation.
"_PACKAGE"

## ------------------------------------------------------------------------
#' Foo bar generic
#'
#' @param x Object to foo.
foobar <- function(x) UseMethod("x")

#' @describeIn foobar Difference between the mean and the median
foobar.numeric <- function(x) abs(mean(x) - median(x))

#' @describeIn foobar First and last values pasted together in a string.
foobar.character <- function(x) paste0(x[1], "-", x[length(x)])

## ------------------------------------------------------------------------
#' Basic arithmetic
#'
#' @param x,y numeric vectors.
add <- function(x, y) x + y

#' @rdname add
times <- function(x, y) x * y

## ------------------------------------------------------------------------
#' Basic arithmetic
#'
#' @param x,y numeric vectors.
#' @name arith
NULL

#' @rdname arith
add <- function(x, y) x + y

#' @rdname arith
times <- function(x, y) x * y

## ------------------------------------------------------------------------
#' @section Warning:
#' Do not operate heavy machinery within 8 hours of using this function.

## ------------------------------------------------------------------------
#' @section Warning:
#' You must not call this function unless ...
#'
#' \subsection{Exceptions}{
#'    Apart from the following special cases...
#' }

## ------------------------------------------------------------------------
#' Basic arithmetic
#'
#' @param x,y numeric vectors.
#' @section Neutral elements:
#'   Addition: 0.
add <- function(x, y) x + y

#' @rdname add
#' @section Neutral elements:
#'   Multiplication: 1.
times <- function(x, y) x * y

## ------------------------------------------------------------------------
#' @backref src/file.cpp
#' @backref src/file.h

