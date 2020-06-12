
find_topic_in_package <- function(pkg, topic) {
  # This is needed because we have the escaped text here, and parse_Rd will
  # un-escape it properly.
  raw_topic <- str_trim(tools::parse_Rd(textConnection(topic))[[1]][1])
  basename(utils::help((raw_topic), (pkg))[1])
}

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
    } else {
      paste0("[=", filename[1], "]")
    }
  }

  str_replace_all(
    rd,
    regex(paste0("(\\[=)?", id, "(.*?)", id, "(\\])?")),
    function(str) fix_link_to_file(str, linkmap)
  )
}
