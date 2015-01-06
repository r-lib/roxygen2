# Re-build all out of date vignettes
# tools::buildVignettes(dir = ".") rebuilds all vignettes - need to rebuild
# only those that have changed
#
# Respect makefile if present

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
  message("Updating vignettes")
  # If has makefile use that
  # make <- Sys.getenv("MAKE", "make"), setwd, system(make)
  # any(grepl("^clean:", readLines("Makefile", warn = FALSE)))) system(paste(make, "clean"))

  vigs <- tools::pkgVignettes(dir = pkg_path)
  invisible(vapply(vigs$docs, vign_update, logical(1)))
}

mtime <- function(x) {
  max(file.info(x)$mtime)
}

vignette_roclet <- function() {
  new_roclet(list, "vignette")
}

#' @export
roc_process.vignette <- function(roclet, parsed, base_path, options = list()) {
  ns <- unlist(lapply(partita, ns_process_partitum)) %||% character()
  sort_c(unique(ns))
}
