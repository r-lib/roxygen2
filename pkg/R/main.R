source('list.R')
source('parse.R')

FILE <- 'example.R'

argv <- commandArgs(trailingOnly=T)
argc <- length(argv)
file <- ifelse(argc > 0, car(argv), FILE)

parse.file(file)
