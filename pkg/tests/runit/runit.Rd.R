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
                  checkTrue(is.null(roclet$parse.parsed(parse.text("")))))

test.naked.roxygen <- function()
  check.Rd.roclet(function(roclet)
                  checkTrue(is.null(roclet$parse.parsed
                                    (parse.text("roxygen()")))))

test.name.from.assignment <- function()
  check.Rd.output('a <- 2',
                  output=c('\\name{a}',
                    '\\alias{a}',
                    '\\title{a}'))

test.name.overriding.assignment <- function()
  check.Rd.output("#' @name b
                   a <- 2",
                  output=c('\\name{b}',
                    '\\alias{b}',
                    '\\title{b}'))

test.implicit.usage.from.formals <- function()
  check.Rd.output("a <- function(a=1) {}",
                  output=c("\\name{a}",
                    "\\alias{a}",
                    "\\title{a}",
                    "\\usage{a(a=1)}"))

test.explicit.usage <- function()
  check.Rd.output("#' @usage a(a=2)
                   a <- function(a=1) {}",
                  output=c("\\name{a}",
                    "\\alias{a}",
                    "\\title{a}",
                    "\\usage{a(a=2)}"))

test.params <- function()
  check.Rd.output("#' @param a an incipit letter
                   #' @param z a terminal letter
                   a <- function(a=1, z=2) {}",
                  output=c("\\name{a}",
                    "\\alias{a}",
                    "\\title{a}",
                    "\\usage{a(a=1, z=2)}",
                    "\\arguments{\\item{a}{an incipit letter}",
                    "\\item{z}{a terminal letter}}"))

test.description <- function()
  check.Rd.output("#' description
                   roxygen()",
                  output="\\description{description}")

test.description.details <- function()
  check.Rd.output("#' description
                   #'
                   #' details
                   roxygen()",
                  output=c("\\description{description}",
                    "\\details{details}"))

test.splitting.keys <- function()
  check.Rd.output("#' @keywords a b
                   #' @aliases a b
                   roxygen()",
                  output=c("\\keyword{a}",
                    "\\keyword{b}",
                    "\\alias{a}",
                    "\\alias{b}"))

test.generic.keys <- function()
  check.Rd.output("#' @references test
                   #' @note test
                   #' @author test
                   #' @seealso test
                   #' @concept test
                   roxygen()",
                  output=c("\\references{test}",
                    "\\note{test}",
                    "\\author{test}",
                    "\\seealso{test}",
                    "\\concept{test}"))

test.title.from.description <- function()
  check.Rd.output("#' Description with sentence. That continueth.
                   a <- 2",
                  output=c("\\name{a}",
                    "\\alias{a}",
                    "\\title{Description with sentence.}",
                    paste("\\description{Description with sentence.",
                          "That continueth.}")))

test.override.title <- function()
  check.Rd.output("#' Would be title
                   #' @title Overridden title
                   roxygen()",
                  output=c("\\title{Overridden title}",
                    "\\description{Would be title}"))

test.question.mark.end.of.sentence <- function()
  check.Rd.output("#' Is a number odd?
                   is.odd <- function(a) {}",
                  output=c('\\name{is.odd}',
                    '\\alias{is.odd}',
                    '\\title{Is a number odd?}',
                    '\\usage{is.odd(a)}',
                    '\\description{Is a number odd?}'))

test.ellipsis.on.no.period <- function()
  check.Rd.output("#' Whether a number is odd
                   is.odd <- function(a) {}",
                  output=c('\\name{is.odd}',
                    '\\alias{is.odd}',
                    '\\title{Whether a number is odd...}',
                    '\\usage{is.odd(a)}',
                    '\\description{Whether a number is odd}'))

test.todo <- function()
  check.Rd.output("#' @TODO test this
                   roxygen()",
                  output=c('\\section{TODO}{test this}'))
