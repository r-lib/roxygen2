library(Rgraphviz)

PKG.DIR <- 'pkg'

argv <- commandArgs(trailingOnly=T)
argc <- length(argv)
pkg.dir <- if (argc > 0) argv[[1]] else PKG.DIR

sources <- c('%s/R/roxygen.R',
             '%s/R/functional.R',
             '%s/R/list.R',
             '%s/R/parse.R',
             '%s/R/string.R',
             '%s/R/roclet.R',
             '%s/R/Rd.R',
             '%s/R/namespace.R',
             '%s/R/collate.R',
             '%s/R/roxygenize.R',
             '%s/R/description.R',
             '%s/R/callgraph.R')

for (source in sources)
  source(sprintf(source, pkg.dir))

roxygenize(pkg.dir)
