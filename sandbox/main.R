source('../R/functional.R')
source('../R/list.R')
source('../R/string.R')
source('../R/parse.R')

FILE <- 'example-S4-person.R'

argv <- commandArgs(trailingOnly=T)
argc <- length(argv)
file <- ifelse(argc > 0, car(argv), FILE)

str(parse.file(file))
