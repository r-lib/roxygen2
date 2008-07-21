library(RUnit)

source('../R/functional.R')
source('../R/list.R')
source('../R/string.R')
source('../R/parse.R')
source('../R/roclet.R')
source('../R/collate.R')
source('../R/namespace.R')
source('../R/Rd.R')

runTestFile('tests.R')
