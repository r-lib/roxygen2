
# Roxygen base:
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
  source(sprintf(source, '..'))

# Changes:
library(tools)

source('../R/print.Rd.R')
source('../R/merge.Rd.R')
source('../R/Rd.R')

rd <- make.Rd.roclet(subdir='.')
rd$parse('example-pseudoprime.R')

p <- parse_Rd('is.pseudoprime.Rd')
p

parse_Rd('fermat.Rd')

p1 <- parse_Rd('fermat.test.Rd')
p2 <- parse_Rd('is.pseudoprime.Rd')

merge.Rd(p1, p2)


merge.Rd(p2, p1)[[25]][[1]]
