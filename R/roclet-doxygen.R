# @title Replace a value for a given tag
# @description On a configuration file as a list of strings in memory:
#    scan each line, detect if the given tag is present and
#    and change the corresponding value.
#    If no line has this tag, add a line with the tag and the value at the end.
#    If several lines match the tag, returns a warning.
# @param file_strings A vector with each string containing a line of the file
# @param tag The tag to be searched for
# @param new_val The new value for the tag
# @return The vector of strings with the new value
# @keywords internal
replace_tag <- function(file_strings, tag, new_val) {
    i_line <- grep(paste0("^", tag, "\\>"), file_strings)
    n_lines <- length(i_line)
    if (n_lines == 0) {
        line <- paste0(tag, "\t= ", new_val)
        i_line <- length(file_strings) + 1
    } else if (n_lines > 0) {
        line <- gsub("=.*", paste0("= ", new_val), file_strings[i_line])
        if (n_lines > 1) {
            warning(paste0("File has", n_lines, "for key", tag, "check it up manually"))
    }
    }
    file_strings[i_line] <- line
    return(file_strings)
}
# check if doxygen is functional
check_doxygen <- function(test_command="doxygen -v"){
    version_doxygen <-try(system(test_command,intern=TRUE,ignore.stderr=TRUE),silent=TRUE)
    if(class(version_doxygen)=="try-error"){
        warning(paste("doxygen doesn't seem installed:",test_command,"returned an error."))
        return(FALSE)
    }else{
        return(TRUE)
    }
}
# launch doxygen documentation
doxygen <- function(doxy_file){
    doxygen_ok <- check_doxygen()
    if(doxygen_ok){
        system(paste0("doxygen ", shQuote(normalizePath(doxy_file))))
    }
}

# prepare a folder for doxygen if doxygen is installed
prepare_folder <- function(dox_dir){
    # prepare the folder for doxygen config file
    if (!file.exists(dox_dir)) {
        dir.create(dox_dir, recursive = TRUE)
    }

    return(TRUE)
}
#' Prepares the R package structure for use with doxygen
#' @description Makes a configuration file in inst/doxygen
#'     and sets a few options:
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
#' use_doxygen()
#' }
#' @keywords internal
use_doxygen <- function(doxy_file){
    dox_dir <- dirname(doxy_file)
    doxy_file_name <- basename(doxy_file)
    
    # checks that doxygen is installed
    doxygen_ok <- check_doxygen()

    # prepare folder tree if needed
    if(doxygen_ok){
        prepare_folder(dox_dir)

        # prepare the configuration file for doxygen
        system(paste0("doxygen -g ", shQuote(normalizePath(doxy_file))))

        config <- readLines(doxy_file)
        config <- replace_tag(config, "EXTRACT_ALL", "YES")
        config <- replace_tag(config, "INPUT", "src/")
        config <- replace_tag(config, "OUTPUT_DIRECTORY", dox_dir)
        cat(config, file = doxy_file, sep = "\n")
    }

    return(NULL)
}

#' Makes doxygen documentation
#' @description Roclet making documentation from
#'    doxygen instructions in src/, note that a doxygen configuration file
#'    inst/doxygen/Doxyfile is created. You can edit it but edits would be lost
#'    if you call this function again or call to the clean method of this roclet. 
#' @details The function \code{roc_output.doxygen} verifies the existence of a doxygen structure, if
#'    needed it creates it and run doxygen. 
#'    doxygen should be previously installed
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
    doxygen_update_all(base_path)
}

#' @export
roc_output.doxygen <- function(roclet, results, base_path, options = list(), check = TRUE) {
}
#' @export
clean.doxygen <- function(roclet, results, base_path, options = list(), check = TRUE) {
    doxygen_path <- file.path(base_path, "inst", "doxygen")
    unlink(doxygen_path,recursive=TRUE)
}

# @description The workhorse of the doxygen roclet, making if necessary a doxygen configuration file
#      and launching doxygen on it
# @param pkg_path The root of the package to be treated
# @param compiled_code Is there compiled code, hence should doxygen be ran. If NULL, doxygen will    
#        run only if a "src" folder exists. 
# @keywords internal
doxygen_update_all <- function(pkg_path = ".", compiled_code = NULL) {
    doxygen_path <- file.path(pkg_path, "inst", "doxygen")
    doxy_file <- file.path(doxygen_path, "Doxyfile")

    # do not run doxygen if no sources folder, unless specified
    if (is.null(compiled_code)) {
        compiled_code <- file.exists(file.path(pkg_path, "src"))
    }

    if (compiled_code) {
        if (!file.exists(doxy_file)) {
            use_doxygen(doxy_file = doxy_file)
        }
        doxygen(doxy_file=doxy_file)
    }
}
