#' @include roxygen.R
#' @include Rd.R
#' @include namespace.R
#' @include collate.R
roxygen()

#' Whither to copy package
ROXYGEN.DIR <- '%s.roxygen'

#' Whither to copy Rds
MAN.DIR <- 'man'

#' Whither to copy installables
INST.DIR <- 'inst'

#' Whither to install docs
DOC.DIR <- 'doc'

#' Whence to copy source code
R.DIR <- 'R'

#' Whither to copy namespace
NAMESPACE.FILE <- 'NAMESPACE'

#' Whither to copy collate
DESCRIPTION.FILE <- 'DESCRIPTION'

#' Recursively copy a directory thither; optionally unlinking
#' the target first; optionally overwriting; optionally
#' verbalizing.
#' @param source the source directory
#' @param target the target directory
#' @param unlink.target delete target directory first?
#' @param overwrite overwrite target files?
#' @param verbose verbalize transaction?
#' @return \code{NULL}
#' @note Not tested on non-linux platforms
copy.dir <- function(source,
                     target,
                     unlink.target=FALSE,
                     overwrite=FALSE,
                     verbose=FALSE) {
  if (unlink.target)
    unlink(target, recursive=TRUE)
  files <- list.files(source, full.name=TRUE, recursive=TRUE)
  for (source.file in files) {
    promoted.file <- sub('[^/\\]*(/|\\\\)', '', source.file)
    target.file <- file.path(target, promoted.file)
    target.dir <- dirname(target.file)
    ## Could create, instead, a list of unique directories in
    ## Theta(n).
    dir.create(target.dir, recursive=TRUE, showWarnings=FALSE)
    if (verbose)
      cat(sprintf('%s -> %s', source.file, target.file), '\n')
    file.copy(source.file, target.file, overwrite=overwrite)
  }
}

#' Process a package with the Rd, namespace and collate roclets.
#' @param package.dir the package's top directory
#' @param copy.package if R.utils is present, copies the package
#' over before adding/manipulating files.
#' @return \code{NULL}
#' @callGraph
#' @callGraphDepth 1
roxygenize <- function(package.dir,
                       copy.package=TRUE) {
  roxygen.dir <- sprintf(ROXYGEN.DIR, package.dir)
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
             unlink.target=TRUE,
             overwrite=TRUE,
             verbose=FALSE)

  for (dir in skeleton) dir.create(dir, showWarnings=FALSE)
  r.dir <- file.path(package.dir, R.DIR)
  files <- as.list(list.files(r.dir, recursive=TRUE, full.names=TRUE))
  Rd <- make.Rd.roclet(man.dir)
  do.call(Rd$parse, files)
  namespace <- make.namespace.roclet(namespace.file)
  do.call(namespace$parse, files)
  collate <- make.collate.roclet(merge.file=package.description,
                                 target.file=roxygen.description)
  collate$parse.dir(r.dir)
  callgraph <-
    make.callgraph.roclet(description.dependencies(package.description),
                          doc.dir)
  do.call(callgraph$parse, files)
                          
}
