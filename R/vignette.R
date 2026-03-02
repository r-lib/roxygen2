#' Re-build outdated vignettes
#'
#' @description
#' This roclet rebuilds outdated vignettes with [tools::buildVignette],
#' but we no longer recommend it because we no longer recommend storing
#' built vignettes in a package.
#'
#' By default, it will rebuild all vignettes if the source file is newer than
#' the output pdf or html. (This means it will automatically re-build the
#' vignette if you change the vignette source, but _not_ when you
#' change the R code). If you want finer control, add a Makefile to
#' `vignettes/` and roxygen2 will use that instead.
#'
#' To prevent RStudio from re-building the vignettes again when checking
#' your package, add `--no-build-vignettes` to the "Build Source Package"
#' field in your project options.
#'
#' @keywords internal
#' @export
vignette_roclet <- function() {
  roclet("vignette")
}

#' @export
roclet_process.roclet_vignette <- function(x, blocks, env, base_path) {}

#' @export
roclet_output.roclet_vignette <- function(x, results, base_path, ...) {
  vign_update_all(base_path)
}

# Determine if a vignette is out-of-date; i.e. it has no related files, or
# any of the related files are older than the vignette.
vign_outdated <- function(vign) {
  vign <- normalizePath(vign, mustWork = TRUE)
  name <- tools::file_path_sans_ext(basename(vign))

  # Currently, the final product of a vignette can only be pdf or html
  related <- dir(
    dirname(vign),
    pattern = paste0(name, "\\.(pdf|html)$"),
    full.names = TRUE
  )
  related <- setdiff(related, vign)

  length(related) == 0 || mtime(vign) > mtime(related)
}

vign_update <- function(vign) {
  if (!vign_outdated(vign)) {
    return(FALSE)
  }

  cli::cli_inform("Rebuilding {.file {basename(vign)}}")
  output <- tools::buildVignette(
    vign,
    dirname(vign),
    tangle = FALSE,
    clean = FALSE
  )

  TRUE
}

vign_update_all <- function(pkg_path) {
  vig_path <- file.path(pkg_path, "vignettes")
  if (!file.exists(vig_path)) {
    return()
  }

  if (file.exists(file.path(vig_path, "Makefile"))) {
    cli::cli_inform("Updating vignettes with make")

    make <- Sys.getenv("MAKE", "make")
    old <- setwd(vig_path)
    on.exit(setwd(old), add = TRUE)

    system(make)
  } else {
    cli::cli_inform("Updating vignettes")

    vigs <- tools::pkgVignettes(dir = pkg_path)
    invisible(map_lgl(vigs$docs, vign_update))
  }
}

mtime <- function(x) {
  max(file.info(x)$mtime)
}
