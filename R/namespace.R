# Parse NAMESPACE into convenient format
read.namespace <- function(file) {
  ns <- readLines(file)
  str_trim(ns)
}

#' grab the packages to be Imported from the NAMESPACE file.
#'
#' @param file the path to a NAMESPACE file
#' @return a \code{character} \code{vector} of pacakge imports, or a 
#' character(0) if none were found
#' @author Mark Cowley, 2012-07-09
#' @noRd
namespace.file.imports <- function(file) {
  ns <- read.namespace(file)
  namespace.imports(ns)
}

#' grab the packages to be Imported from the namespace_roclet.
#'
#' @param ns a character vector of values from running namespace_roclet.
#' @return a \code{character} \code{vector} of pacakge imports, or a 
#' character(0) if none were found
#' @author Mark Cowley, 2012-07-09
#' @noRd
namespace.imports <- function(ns) {
  ns <- grep("^import", ns, value = TRUE)
  if (length(ns) > 0) {
    # 4 examples
    # import(ggplot2)
    # importFrom(filehash,dbInsert)
    # importClassesFrom(lumi,LumiBatch)
    # importMethodsFrom(lumi,detection)

    ns.import.pkgs <- sub("import.*\\(", "", sub(",.*", "", ns))
    ns.import.pkgs <- sub("\\)", "", ns.import.pkgs)
    ns.import.pkgs <- with_locale("C", sort(unique(ns.import.pkgs)))
  }
  else {
    ns.import.pkgs <- character()
  }
  ns.import.pkgs
}
