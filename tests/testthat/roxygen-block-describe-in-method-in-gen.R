#' Zap generic
#'
#' @param x Object to zap.
zap <- function(x) UseMethod("x")

#' @describeIn zap Difference between the mean and the median
zap.numeric <- function(x) abs(mean(x) - median(x))

#' @describeIn zap First and last values pasted together in a string.
zap.character <- function(x) paste0(x[1], "-", x[length(x)])

# because this is a method for the wrong generic, it will be listed as a function
#' @describeIn zap Pretty printing of qux objects
print.qux <- function(x) print("I'm a qux.")

#' @describeIn zap Helper function to zap
zap_helper <- function(x) print("Zap zap.")
