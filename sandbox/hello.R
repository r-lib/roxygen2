library(roxygen)

register.preref.parsers(parse.name, 'hello')

make.hello.roclet <- function() {
  roclet <- make.roclet()

  parse.hello <- function(key, expression)
    cat(sprintf("Hi, %s!\n", expression))

  roclet$register.parser('hello', parse.hello)

  roclet
}

roclet <- make.hello.roclet()
roclet$parse.parsed(parse.text("#' @hello world",
                               "NA"))
