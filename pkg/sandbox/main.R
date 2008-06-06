source('../R/list.R')
source('../R/parse.R')

FILE <- 'example.R'

argv <- commandArgs(trailingOnly=T)
argc <- length(argv)
file <- ifelse(argc > 0, car(argv), FILE)

l <- parse.file(file)
str(l)


l1 <- parse.file('example-S4-person.R')
