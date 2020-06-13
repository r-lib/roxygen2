
#' Find the Rd file of a topic
#'
#' @param pkg Package to search in, or `NA` if no package was specified.
#'   If the same as the dev package, then we treat it as `NA`.
#' @param topic Topic to search for. This is the escaped, so it is `"\%\%"` and
#'   not `"%%"`.
#' @param tag The roxy tag object that contains the link. We use this for
#'   better warnings, that include the file name and line number (of the tag).
#' @return String. File name to link to.
#'
#' @details
#' If `pkg` is `NA` or the package being documented, we'll just leave the
#' topic alone.
#'
#' If `pkg` is not `NA` and not the package being documented (the _dev_
#' package), then we need to be able to find the Rd file. If we can't, that's
#' a warning and the link is left untouched. This typically happens when the
#' linked package is not installed or cannot be loaded.
#'
#' @noRd

find_topic_filename <- function(pkg, topic, tag) {
  tag <- tag %||% list(file = NA, line = NA)
  if (is.na(pkg) || identical(roxy_meta_get("current_package"), pkg)) {
    topic
  } else {
    path <- tryCatch(
      find_topic_in_package(pkg, topic),
      error = function(err) {
        roxy_tag_warning(tag, "Link to unavailable package: ", pkg, ". ", err$message)
        topic
      }
    )
    if (is.na(path)) {
      roxy_tag_warning(tag, "Link to unknown topic: ", topic, " in package ", pkg)
      topic
    } else {
      path
    }
  }
}

#' Find a help topic in a package
#'
#' This is used by both `find_topic_filename()` and
#' `format.rd_section_reexport()` that creates the re-exports page. The error
#' messages are different for the two, so errors are not handled here.
#'
#' @param pkg Package name. This cannot be `NA`.
#' @inheritParams find_topic_filename
#' @return File name if the topic was found, `NA` if the package could be
#'   searched, but the topic was not found. Errors if the package cannot be
#'   searched. (Because it is not installed or cannot be loaded, etc.)
#'
#' @noRd

find_topic_in_package <- function(pkg, topic) {
  # This is needed because we have the escaped text here, and parse_Rd will
  # un-escape it properly.
  raw_topic <- str_trim(tools::parse_Rd(textConnection(topic))[[1]][1])
  basename(utils::help((raw_topic), (pkg))[1])
}
