source('../R/roxygen.R')
source('../R/functional.R')
source('../R/list.R')
source('../R/parse.R')
source('../R/string.R')
source('../R/roclet.R')
source('../R/namespace.R')
source('../R/collate.R')
source('../R/Rd.R')
source('../R/callgraph.R')

FILES <- list('../R/Rd.R')

argv <- commandArgs(trailingOnly=T)
argc <- length(argv)
files <- if (argc > 0) as.list(argv) else FILES

roclet <- make.Rd.roclet()
do.call(roclet$parse, files)


### "Missing quotes in default arguments" bug:

package.skeleton('helloRoxygen', code_files = 'hello-roxygen.R', force = TRUE)

roclet <- make.Rd.roclet()
roclet$parse('helloRoxygen/R/hello-roxygen.R')
