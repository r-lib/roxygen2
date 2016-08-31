#' Re-build outdated vignettes.
#'
#' This rebuilds outdated vignettes with \code{\link[tools]{buildVignette}()}.
#' By default, it will rebuild all vignettes if the source file is newer than
#' the output pdf or html. (This means it will automatically re-build the
#' vignette if you change the vignette source, but \emph{not} when you
#' change the R code). If you want finer control, add a Makefile to
#' \code{vignettes/} and roxygen2 will use that instead.
#'
#' To prevent RStudio from re-building the vignettes again when checking
#' your package, add \code{--no-build-vignettes} to the "Build Source Package"
#' field in your project options.
#'
#' @family roclets
#' @export
vignette_roclet <- function() {
  new_roclet(list, "vignette")
}

#' @export
roc_process.vignette <- function(roclet, parsed, base_path, options = list()) {
}

#' @export
roc_output.vignette <- function(roclet, results, base_path, options = list(),
                                check = TRUE) {

  vign_update_all(base_path)
}

# Determine if a vignette is out-of-date; i.e. it has no related files, or
# any of the related files are older than the vignette.
vign_outdated <- function(vign) {
  vign <- normalizePath(vign, mustWork = TRUE)
  name <- tools::file_path_sans_ext(basename(vign))

  # Currently, the final product of a vignette can only be pdf or html
  related <- dir(dirname(vign), pattern = paste0(name, "\\.(pdf|html)$"),
    full.names = TRUE)
  related <- setdiff(related, vign)

  length(related) == 0 || mtime(vign) > mtime(related)
}

vign_update <- function(vign) {
  if (!vign_outdated(vign)) return(FALSE)

  message("Rebuilding ", basename(vign))
  output <- tools::buildVignette(vign, dirname(vign), tangle = FALSE,
    clean = FALSE)

  TRUE
}

vign_update_all <- function(pkg_path) {
  vig_path <- file.path(pkg_path, "vignettes")
  if (!file.exists(vig_path)) return()

  if (file.exists(file.path(vig_path, "Makefile"))) {
    message("Updating vignettes with make")

    make <- Sys.getenv("MAKE", "make")
    old <- setwd(vig_path)
    on.exit(setwd(old), add = TRUE)

    system(make)
  } else {
    message("Updating vignettes")

    vigs <- tools::pkgVignettes(dir = pkg_path)
    invisible(vapply(vigs$docs, vign_update, logical(1)))
  }
}

mtime <- function(x) {
  max(file.info(x)$mtime)
}
