#' @include Rd.R
#' @include namespace.R
#' @include collate.R
ROXYGEN.DIR <- '%s.roxygen'
MAN.DIR <- 'man'
R.DIR <- 'R'

roxygenize <- function(package.dir) {
  roxygen.dir <- sprintf(ROXYGEN.DIR, package.dir)
  man.dir <- file.path(roxygen.dir, MAN.DIR)
  skeleton <- c(roxygen.dir, man.dir)
  for (dir in skeleton) dir.create(dir, showWarnings=F)
  r.dir <- file.path(package.dir, R.DIR)
  source.files <- list.files(r.dir, recursive=T, full.names=T)
  Rd <- make.Rd.roclet(man.dir)
  do.call(Rd$parse, as.list(source.files))
}
