source('../R/roxygen.R')
source('../R/functional.R')
source('../R/list.R')
source('../R/parse.R')
source('../R/string.R')
source('../R/roclet.R')
source('../R/Rd.R')

FILES <- list('../R/Rd.R')

argv <- commandArgs(trailingOnly=T)
argc <- length(argv)
files <- if (argc > 0) as.list(argv) else FILES

roclet <- make.Rd.roclet()
do.call(roclet$parse, files)
