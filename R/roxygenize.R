#' @include roxygen.R
#' @include namespace.R
#' @include collate.R
NULL

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
                     target = source,
                     unlink.target=FALSE,
                     overwrite=FALSE,
                     verbose=FALSE) {
  if (unlink.target)
    unlink(target, recursive=TRUE)
  files <- list.files(source,
                      full.name=TRUE,
                      recursive=TRUE,
                      all.files=TRUE)
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
#' @param roxygen.dir where to create roxygen output; defaults to
#' \file{package.roxygen}.
#' @param copy.package copies the package over before
#' adding/manipulating files.
#' @param overwrite overwrite target files
#' @param unlink.target unlink target directory before
#' processing files
#' @param use.Rd2 use the Rd2 roclet
#' @return \code{NULL}
#' @TODO Options to enable/disable specific roclet
#' (\command{--no-callgraphs}, etc.)
#' @export roxygenize roxygenise
roxygenize <- function(package.dir,
                       roxygen.dir=package.dir,
                       copy.package=package.dir != roxygen.dir,
                       overwrite=TRUE,
                       unlink.target=FALSE,
                       roclets=c("had", "collate")) {

  skeleton <- c(roxygen.dir,
                file.path(roxygen.dir, MAN.DIR),
                file.path(roxygen.dir, INST.DIR),
                file.path(roxygen.dir, INST.DIR, DOC.DIR))

  if (copy.package)
    copy.dir(package.dir,
             roxygen.dir,
             unlink.target=unlink.target,
             overwrite=overwrite,
             verbose=FALSE)

  for (dir in skeleton) dir.create(dir,
                                   recursive=TRUE,
                                   showWarnings=FALSE)
  
  roclets <- paste("make.", roclets, ".roclet", sep = "")
  for (roclet in roclets) {
    maker <- match.fun(roclet)
    maker(package.dir, roxygen.dir)$parse.dir()
  }
}

roxygenise <- roxygenize