library(testthat)
library(roxygen2)

if (requireNamespace("xml2")) {
  test_check("roxygen2", reporter = MultiReporter$new(reporters = list(JunitReporter$new(file = "test-results.xml"), CheckReporter$new())))
} else {
  test_check("roxygen2")
}
