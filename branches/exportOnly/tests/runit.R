check.roclet <- function(make.roclet, test) {
  roclet <- make.roclet()
  test(roclet)
}

check.output <- function(..., make.roclet, output)
  check.roclet(make.roclet,
               test=function(roclet) {
                 parsed <- capture.output(roclet$parse.parsed(parse.text(...)))
                 checkEquals(parsed, output, msg=parsed)
               })

if (require('RUnit')) {
  library(roxygen)
  roclets <- defineTestSuite('roclets', 'runit')
  results <- runTestSuite(roclets)
  printTextProtocol(results)
  errors <- getErrors(results)
  if (errors$nFail > 0 || errors$nErr > 0)
    stop('roclet unit suite failed')
}
