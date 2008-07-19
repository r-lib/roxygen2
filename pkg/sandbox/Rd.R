source('../R/functional.R')
source('../R/list.R')
source('../R/parse.R')
source('../R/strings.R')
source('../R/Rd.R')

FILE <- 'example-Rd-nlm.R'

argv <- commandArgs(trailingOnly=T)
argc <- length(argv)
file <- ifelse(argc > 0, car(argv), FILE)

Rd(parse.file(file))
