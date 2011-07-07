source('../R/roxygen.R')
source('../R/functional.R')
source('../R/list.R')
source('../R/string.R')
source('../R/parse.R')
source('../R/roclet.R')
source('../R/namespace.R')
source('../R/collate.R')
source('../R/Rd.R')
source('../R/callgraph.R')

FILES <- list('example-function-mcpi.R',
              'example-S3-mcpi.R',
              'example-S4-person.R')

argv <- commandArgs(trailingOnly=T)
argc <- length(argv)
files <- if (argc > 0) as.list(argv) else FILES

roclet <- make.namespace.roclet()
do.call(roclet$parse, files)
