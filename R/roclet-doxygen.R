# @title Replace a value for a given tag 
# @description On a configuration file as a list of strings in memory 
#    scan each line, detect if the given tag is present and 
#    and change the corresponding value.  
#    If no line has this tag, add a line with the tag and the value at the end.
#    If several lines match the tag, returns a warning.
# @param fileStrings A vector with each string containing a line of the file
# @param tag The tag to be searched for 
# @param newVal The new value for the tag
# @return The vector of strings with the new value
# @keywords internal 
ReplaceTag <- function(fileStrings, tag, newVal) {
    iLine <- grep(paste0("^", tag, "\\>"), fileStrings)
    nLines <- length(iLine)
    if (nLines == 0) {
        line <- paste0(tag, "\t= ", newVal)
        iLine <- length(fileStrings) + 1
    } else if (nLines > 0) {
        line <- gsub("=.*", paste0("= ", newVal), fileStrings[iLine])
        if (nLines > 1) {
            warning(paste0("File has", nLines, "for key", tag, "check it up manually"))
    }
    }
    fileStrings[iLine] <- line
    return(fileStrings)
}
#' Prepares the R package structure for use with doxygen
#' @description Makes a configuration file in inst/doxygen
#'     and set a few options: 
#'     \itemize{
#'        \item{EXTRACT <- ALL = YES}
#'        \item{INPUT = src/}
#'        \item{OUTPUT <- DIRECTORY = inst/doxygen/}
#'     }
#' @param rootFolder The root of the R package
#' @note Please note this implies the installation of doxygen on the system, 
#'       accessible on the command line by `doxygen`
#' @return NULL
#' @examples 
#' \dontrun{
#' doxygen_init()
#' }
#' @keywords internal
doxygen_init <- function(pkg_path, doxy_file) {
    doxDir <- dirname(doxy_file)
    doxyFileName <- basename(doxy_file)
    
    initFolder <- getwd()
    if (pkg_path != ".") {
        setwd(pkg_path)
    }
    rootFileYes <- length(grep("DESCRIPTION", dir())) > 0
    # prepare the doxygen folder
    if (!file.exists(doxDir)) {
        dir.create(doxDir, recursive = TRUE)
    }
    setwd(doxDir)
    
    # prepare the doxygen configuration file
    system(paste0("doxygen -g ", doxyFileName))
    doxyfile <- readLines(doxyFileName)
    doxyfile <- ReplaceTag(doxyfile, "EXTRACT_ALL", "YES")
    doxyfile <- ReplaceTag(doxyfile, "INPUT", "src/")
    doxyfile <- ReplaceTag(doxyfile, "OUTPUT_DIRECTORY", doxDir)
    cat(doxyfile, file = doxyFileName, sep = "\n")
    setwd(initFolder)
    return(NULL)
}

#' Makes doxygen documentation
#' @description Roclet making documentation from  
#'    doxygen instructions in src/, note that a doxygen configuration file 
#'    inst/doxygen/Doxyfile is created. You can edit it if you wish but it would be 
#'    removed by a call to the call method of this roclet.
#'    
#' @details The function \code{roc_output.doxygen} verifies the existence of a doxygen structure, if
#'    needed create it 
#'    and run doxygen. doxygen should be previously installed 
#'    and accessible in the command line of the system as `doxygen`
#' @examples
#' \dontrun{
#' doxygen_roclet()
#' }
#' @export
doxygen_roclet <- function() {
    new_roclet(list, "doxygen")
}
#' @export
roc_process.doxygen <- function(roclet, parsed, base_path, options = list()) {
}

#' @export
roc_output.doxygen <- function(roclet, results, base_path, options = list(), check = TRUE) {
    doxygen_update_all(base_path)
}
#' @export
clean.doxygen <- function(roclet, results, base_path, options = list(), check = TRUE) {
    doxygen_path <- file.path(pkg_path, "inst", "doxygen")
    unlink(doxygen_path)
    
    
}
#' @description The workhorse of the doxygen roclet, making if necessary a doxygen configuration file
#'      and launching doxygen on it
#' @param pkg_path The root of the package to be treated
#' @param doxygen A boolean: should doxygen be ran on documents in src?
#'     the default is TRUE if a src folder exist and FALSE if not 
#' @keywords internal
doxygen_update_all <- function(pkg_path = ".", compiled_sources = NULL) {
    doxygen_path <- file.path(pkg_path, "inst", "doxygen")
    doxy_file <- file.path(doxygen_path, "Doxyfile")
    
    if (is.null(compiled_sources)) {
        compiled_sources <- file.exists(file.path(pkg_path, "src"))
    }
    
    if (compiled_sources) {
        if (!file.exists(doxy_file)) {
            doxygen_init(pkg_path = ".", doxy_file = doxy_file)
        }
        system(paste0("doxygen ", doxy_file))
    }
} 
