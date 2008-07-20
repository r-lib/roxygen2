source('../R/functional.R')
source('../R/list.R')
source('../R/parse.R')
source('../R/string.R')
source('../R/roclet.R')
source('../R/collate.R')

FILES <- list('collate/belt.R',
              'collate/jacket.R',
              'collate/pants.R',
              'collate/shirt.R',
              'collate/shoes.R',
              'collate/socks.R',
              'collate/tie.R',
              'collate/undershorts.R',
              'collate/watch.R')

argv <- commandArgs(trailingOnly=T)
argc <- length(argv)
files <- if (argc > 0) as.list(argv) else FILES

roclet <- make.collate.roclet()
do.call(roclet$parse, files)
