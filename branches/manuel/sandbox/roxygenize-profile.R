library(proftools)

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

PKG.DIR <- 'pkg'

argv <- commandArgs(trailingOnly=T)
argc <- length(argv)
pkg.dir <- if (argc > 0) car(argv) else PKG.DIR

profile.out <- 'roxygenize-profile.out'
profile.table <- 'roxygenize-profile.txt'
dot.out <- 'roxygenize-callgraph.dot'
dot.png <- 'roxygenize-callgraph.png'

Rprof(profile.out)
roxygenize(pkg.dir)
Rprof(NULL)

cat(system(sprintf('R CMD Rprof %s', profile.out),
           intern=T),
    sep='\n',
    file=profile.table)

profileCallGraph2Dot(readProfileData(profile.out),
                     filename=dot.out)
system(sprintf('dot -o %s -Tpng %s',
               dot.png,
               dot.out))
