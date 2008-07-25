check.Rd.roclet <- Curry(check.roclet,
                         make.roclet=make.Rd.roclet)

check.Rd.output <- Curry(check.output,
                         make.roclet=make.Rd.roclet)

test.example.files <- function()
  check.Rd.output("#' @example runit/Rd-example-1.R
                   #' @example runit/Rd-example-2.R
                   roxygen()",
                  output=c("\\examples{example <- 'example1'",
                    "example <- 'example2'}"))

test.free.example <- function()
  check.Rd.output("#' @examples a <- 2
                   roxygen()",
                  output="\\examples{a <- 2}")

test.free.example.overriding.example.file <- function()
  check.Rd.output("#' @example runit/Rd-example-1.R
                   #' @examples a <- 2
                   roxygen()",
                  output="\\examples{a <- 2}")

test.blank.file <- function()
  check.Rd.roclet(function(roclet)
                  is.null(roclet$parse.parsed(parse.text(""))))

test.naked.roxygen <- function()
  check.Rd.roclet(function(roclet)
                  is.null(roclet$parse.parsed(parse.text("roxygen()"))))

test.name.from.assignment <- function()
  check.Rd.output('a <- 2',
                  output='\\name{a}')

test.name.overriding.assignment <- function()
  check.Rd.output("#' @name b
                   a <- 2",
                  output='\\name{b}')

test.implicit.usage.from.formals <- function()
  check.Rd.output("a <- function(a=1) {}",
                  output=c("\\name{a}",
                    "\\usage{a(a=1)}"))

test.explicit.usage <- function()
  check.Rd.output("#' @usage a(a=2)
                   a <- function(a=1) {}",
                  output=c("\\name{a}",
                    "\\usage{a(a=2)}"))
