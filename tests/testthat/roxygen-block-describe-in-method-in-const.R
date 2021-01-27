#' Class constructor
#'
#' @param x Object to convert to class `foo`.
# lacking a formal class definition, this needs to the same name as below classes
foo <- function(x) structure(x, class = "foo")

#' @describeIn foo Pretty printing in the console
print.foo <- function(x) print("I'm a foo.")

#' @describeIn foo Pretty formatting
format.foo <- function(x) format("I'm still a foo.")

# this will be treated as a normal function, because it's the wrong class
#' @describeIn foo Pretty formatting for another class
format.bar <- function(x) format("I'm a bar.")

#' @describeIn foo Validation for foo objects
is_foo <- function(x) if (FALSE) stop("I'm not a good foo.")

#' Class constructor with disambiguated class
#' 
#' @param x Object to convert to class `roxygen2_baz`.
baz <- function(x) structure(x, class = "roxygen2_baz")

#' @describeIn baz Pretty printing in the console
print.roxygen2_baz <- function(x) print("I'm a baz.")

#' @describeIn baz Pretty formatting
format.quuz_baz <- function(x) print("I don't belong to the above constructor.")
