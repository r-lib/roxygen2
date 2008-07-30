if (!file.exists('pkg/R/parse.R'))
  stop('Run one directory above `pkg\'.')

source('pkg/R/roxygen.R')
source('pkg/R/functional.R')
source('pkg/R/list.R')
source('pkg/R/parse.R')
source('pkg/R/string.R')
source('pkg/R/roclet.R')
source('pkg/R/Rd.R')
source('pkg/R/namespace.R')
source('pkg/R/collate.R')
source('pkg/R/roxygenize.R')
source('pkg/R/description.R')

PKG.DIR <- 'pkg'

argv <- commandArgs(trailingOnly=T)
argc <- length(argv)
pkg.dir <- if (argc > 0) car(argv) else PKG.DIR

roxygenize(pkg.dir)
