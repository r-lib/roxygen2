#' @include Rd.R
#' @include namespace.R
#' @include collate.R
roxygen()

#' Whither to copy package
ROXYGEN.DIR <- '%s.roxygen'

#' Whither to copy Rds
MAN.DIR <- 'man'

#' Whence to copy source code
R.DIR <- 'R'

#' Process a package with the Rd, namespace and collate roclets.
#' @param package.dir the package's top directory
#' @param copy.package if R.utils is present, copies the package
#' over before adding/manipulating files.
#' @return \code{NULL}
roxygenize <- function(package.dir,
                       copy.package=TRUE) {
  roxygen.dir <- sprintf(ROXYGEN.DIR, package.dir)
  man.dir <- file.path(roxygen.dir, MAN.DIR)
  skeleton <- c(roxygen.dir, man.dir)

###   if (copy.package) {
###     if (require('R.utils', quietly=T))
###       copyDirectory(package.dir, roxygen.dir, overwrite=TRUE)
###     else
###       warning('`R.utils\' package not present; not copying package.')
###   }

  for (dir in skeleton) dir.create(dir, showWarnings=FALSE)
  r.dir <- file.path(package.dir, R.DIR)
  source.files <- list.files(r.dir, recursive=TRUE, full.names=TRUE)
  Rd <- make.Rd.roclet(man.dir)
  do.call(Rd$parse, as.list(source.files))
}
