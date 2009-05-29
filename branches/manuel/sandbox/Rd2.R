
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

setwd('Z:\\Projects\\Roxygen\\r-forge\\branches\\manuel\\sandbox')

source('../R/Rd_API.R')
source('../R/Rd.R')
source('../R/Rd_merge.R')

roc <- make.Rd.roclet(subdir='.')
roc$parse('example-pseudoprime.R')




### Benchmark package:

roxygenize2 <- function(package.dir,
                        roxygen.dir=NULL,
                        copy.package=TRUE,
                        overwrite=TRUE,
                        unlink.target=FALSE) {
  if (is.null(roxygen.dir)) roxygen.dir <-
    sprintf(ROXYGEN.DIR, package.dir)
  man.dir <- file.path(roxygen.dir, MAN.DIR)
  inst.dir <- file.path(roxygen.dir, INST.DIR)
  doc.dir <- file.path(inst.dir, DOC.DIR)
  namespace.file <- file.path(roxygen.dir, NAMESPACE.FILE)
  package.description <- file.path(package.dir, DESCRIPTION.FILE)
  roxygen.description <- file.path(roxygen.dir, DESCRIPTION.FILE)
  skeleton <- c(roxygen.dir,
                man.dir,
                doc.dir)

  if (copy.package)
    copy.dir(package.dir,
             roxygen.dir,
             unlink.target=unlink.target,
             overwrite=overwrite,
             verbose=FALSE)

  for (dir in skeleton) dir.create(dir,
                                   recursive=TRUE,
                                   showWarnings=FALSE)
  r.dir <- file.path(package.dir, R.DIR)
  files <- as.list(list.files(r.dir,
                              pattern='\\.(R|r)$',
                              recursive=TRUE,
                              full.names=TRUE,
                              all.files=TRUE))
  Rd <- make.Rd.roclet(man.dir)
  do.call(Rd$parse, files)
  Rd$write()
  
  namespace <- make.namespace.roclet(namespace.file)
  do.call(namespace$parse, files)
  collate <- make.collate.roclet(merge.file=package.description,
                                 target.file=roxygen.description)
  collate$parse.dir(r.dir)
  #callgraph <-
  #  make.callgraph.roclet(description.dependencies(package.description),
  #                        doc.dir)
  #do.call(callgraph$parse, files)

  return(Rd)
}                    

setwd('Z:/Research/Benchmarking')

r <- roxygenize2('pkg', roxygen.dir='builds/benchmark')

r <- parse_Rd('builds/benchmark/man/basicplots.Rd')
r <- parse_Rd('builds/benchmark/man/bench-class.Rd')
