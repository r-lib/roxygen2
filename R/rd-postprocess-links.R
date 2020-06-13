
#' Find the Rd file of a topic, or generate a placeholder string, to fill in
#' later
#'
#' @param pkg Package to search in, or `NA` if no package was specified.
#'   If the same as the dev package, then we treat it as `NA`.
#' @param topic Topic to search for. This is the escaped, so it is `"\%\%"` and
#'   not `"%%"`.
#' @param tag The roxy tag object that contains the link. We use this for
#'   better warnings, that include the file name and line number (of the tag).
#' @param force Whether we must always include a file name, even if it matches
#'   the topic. See more below.
#' @return String. File name or placeholder. See details below.
#'
#' @details
#' If `pkg` is not `NA` and not the package being documented (the _dev_
#' package), then we need to be able to find the Rd file. If we can't, that's
#' a warning and the link is left untouched. This typically happens when the
#' linked package is not installed or cannot be loaded.
#'
#' If `pkg` is `NA` that means that the link is unqualified (only the topic is
#' given, its package is not). This typically means a link to the dev package,
#' but not necessarily, given that [utils::help()] is able to look up topics
#' at render time.
#'
#' If `pkg` is not specified then we cannot yet find the Rd file name of the
#' link. In this case we return a placeholder string, that is finalized when
#' the Rd content is created, in the [RoxyTopic] `format()` method, by a
#' call to `fix_links_to_file()` below.
#'
#' The placeholder string looks like this:
#' ```
#' id|force|file|line|topic|id
#' ```
#'
#' * `id`: is a random id that is used to find the links that need
#'   post-processing. It is generated at the beginning of `roxygenize()`, so
#'   it is the same for all placeholders. We use a single id, so we don't
#'   need to keep a dictionary of placeholders and multiple searches. A
#'   single regular expression search finds all placeholders of an Rd file,
#'   see `fix_links_to_file()` below.
#' * `force`: is whether we always need to include a file name. If the link
#'   text is different from the topic name (e.g. most commonly because we are
#'   linking to a function and adding `()`), then this is set to `"1"`.
#'   Otherwise it is set to `"0"`. If it is `"0"`, and we can get away without
#'   including a file name.
#' * `file`: R file name of the link. Can be used for better warnings. To allow
#'   arbitrary file names without throwing off our regex search, this is
#'   URL encoded.
#' * `line`: Line number of the link. Can be used for better warnings.
#'   It seems that this is the line number of the roxygen2 tag, within the
#'   roxygen2 block, which is not great, but we can improve it later.
#' * `topic`: the topic we are linking to, that needs to be mapped to a
#'   file name. Escaped, so it will be `\%\%`, and not `%%`.
#' * `id`: the same random id again, so we can easily identify the start and
#'   end of the placeholder.
#'
#' @noRd

find_topic_filename <- function(pkg, topic, tag, force = TRUE) {
  if (is.na(pkg) || identical(roxy_meta_get("current_package"), pkg)) {
    id <- roxy_meta_get("link_id")
    if (!is.null(id)) {
      # id is only NULL in test cases, not in roxygenize()
      file <- URLencode(basename(tag$file), TRUE, TRUE)
      paste0(id, ",", force + 0, ",", file, ",", tag$line, ",", topic, ",", id)
    } else {
      topic
    }
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

#' Replace placeholders with file names
#'
#' @param rd The text of a manual page.
#' @param linkmap Environment that maps from topic to file name(s). One
#'   topic might link to multiple file names, but we always use the first one.
#'   This is a `@name` if the topic had a name at all. Otherwise it is the
#'   first of the `@aliases`.
#' @return String. `rd`, with the link placeholders filled in.
#'
#' @details
#' TODO: Currently we give a warning for each topic that we cannot find,
#' but we'll change this to a single note.
#'
#' The workhorse is the `fix_link_to_file()` function, that receives the
#' text of the placeholder, usually with the surrounding `[=` ... `]` symbols.
#' (If these are not present, that's a qualified self link to the dev package.)
#'
#' @noRd

fix_links_to_file <- function(rd, linkmap) {
  id <- roxy_meta_get("link_id")
  # This can only be NULL in our test cases
  if (is.null(id)) return(rd)

  fix_link_to_file <- function(str, linkmap) {
    nopkg <- FALSE
    if (substr(str, 1, 1) == "[") {
      nopkg <- TRUE
      str <- substr(str, 3, nchar(str) - 1)
    }
    pieces <- strsplit(str, ",", fixed = TRUE)[[1]]
    topic <- pieces[5]
    filename <- linkmap[[topic]]
    if (length(filename) == 0) {
      roxy_warning(
        "Link to unknown topic '", topic, "'",
        file = utils::URLdecode(pieces[3]),
        line = as.integer(pieces[4])
      )
      filename <- topic
    }
    if (filename[1] == topic && nopkg && pieces[2] == "0") {
      ""
    } else if (nopkg) {
      paste0("[=", filename[1], "]")
    } else {
      filename[1]
    }
  }

  str_replace_all(
    rd,
    regex(paste0("(\\[=)?", id, "(.*?)", id, "(\\])?")),
    function(str) fix_link_to_file(str, linkmap)
  )
}
