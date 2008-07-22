if (require('RUnit')) {
  library(roxygen)
  roclets <- defineTestSuite('roclets', 'runit')
  results <- runTestSuite(roclets)
  printTextProtocol(results)
  errors <- getErrors(results)
  if (errors$nFail > 0 || errors$nErr > 0)
    stop('roclet unit suite failed')
}
