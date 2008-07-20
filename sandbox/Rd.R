source('../R/functional.R')
source('../R/list.R')
source('../R/parse.R')
source('../R/string.R')
source('../R/roclet.R')
source('../R/Rd.R')

FILES <- list('example-Rd-nlm.R')

argv <- commandArgs(trailingOnly=T)
argc <- length(argv)
files <- ifelse(argc > 0, as.list(argv), FILES)

Rd(do.call(parse.file, files))
