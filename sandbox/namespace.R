source('../R/functional.R')
source('../R/list.R')
source('../R/string.R')
source('../R/parse.R')
source('../R/namespace.R')

FILES <- list('example-function-mcpi.R',
              'example-S3-mcpi.R',
              'example-S4-person.R')

argv <- commandArgs(trailingOnly=T)
argc <- length(argv)
files <- if (argc > 0) as.list(argv) else FILES

namespace(do.call(parse.files, files))
