
restricted_markdown <- function(rest) {
  markdown(rest, markdown_tags_restricted)
}

full_markdown <- function(rest) {
  rest <- escape_leading_whitespace(rest)
  rest <- markdown(rest, markdown_tags)
  rest <- unescape_leading_whitespace(rest)
  rest
}

#' @importFrom commonmark markdown_xml
#' @importFrom xml2 read_xml

markdown <- function(text, markdown_tags) {
  esc_text <- escape_rd_for_md(text)
  md <- markdown_xml(esc_text, hardbreaks = TRUE)
  xml <- read_xml(md)
  rd_text <- str_trim(markdown_rparse(xml, markdown_tags))
  unescape_rd_for_md(rd_text, esc_text)
}

#' @importFrom xml2 xml_name xml_type xml_text xml_contents xml_attr
#'   xml_children

markdown_rparse <- function(xml, markdown_tags) {

  ## We have a string, a list of things, or XML stuff

  ## Strings are simply returned
  if (is.character(xml)) return(paste(xml, collapse = ""))

  ## List is iterated over
  if (is(xml, "list")) {
    return(paste(vapply(xml, markdown_rparse, "", markdown_tags = markdown_tags),
                 collapse = ""))
  }

  ## Otherwise it is XML stuff, node set
  if (is(xml, "xml_nodeset")) {
    return(paste(vapply(xml, markdown_rparse, "", markdown_tags = markdown_tags),
                 collapse = ""))
  }

  ## text node, only keep if not entirely whitespace
  if (is(xml, "xml_node") && xml_type(xml) == "text") {
    return(ws_to_empty(xml_text(xml)))
  }

  ## generic node
  if (is(xml, "xml_node") && xml_type(xml) == "element") {
    return(markdown_rparse(markdown_tags[[ xml_name(xml) ]](xml), markdown_tags = markdown_tags))
  }

  warning("Unknown xml object")
}

markdown_tags <- list(

  unknown = function(xml) {
    ## Just ignore the tag, keep the contents
    xml_contents(xml)
  },

  document = function(xml) {
    ## Keep the contents only, tag does nothing
    xml_contents(xml)
  },

  block_quote = function(xml) {
    ## Cannot really format this in Rd, so we just leave it
    xml_contents(xml)
  },

  list = function(xml) {
    ## A list, either bulleted or numbered
    type <- xml_attr(xml, "type")
    if (type == "ordered") {
      list("\n\\enumerate{", xml_contents(xml), "\n}")
    } else {
      list("\n\\itemize{", xml_contents(xml), "\n}")
    }
  },

  item = function(xml) {
    ## A single item within a list. We remove the first paragraph
    ## tag, to avoid an empty line at the beginning of the first item.
    cnts <- xml_children(xml)
    if (xml_name(cnts[[1]]) == "paragraph") {
      cnts <- list(xml_contents(cnts[[1]]), cnts[-1])
    }
    list("\n\\item ", cnts)
  },

  code_block = function(xml) {
    ## Large block of code
    list("\\preformatted{", xml_contents(xml), "}")
  },

  html = function(xml) {
    ## Not sure what to do with this, we'll just leave it
    xml_contents(xml)
  },

  paragraph = function(xml) {
    ## Paragraph
    list("\n\n", xml_contents(xml))
  },

  header = function(xml) {
    ## Not sure what this is, ignore it for now.
    xml_contents(xml)
  },

  hrule = function(xml) {
    ## We cannot have horizontal rules, will just start a new
    ## paragraph instead.
    "\n\n"
  },

  text = function(xml) {
    ## Just ordinary text, leave it as it is.
    xml_text(xml)
  },

  softbreak = function(xml) {
    ## Inserted line break (?).
    "\n"
  },

  linebreak = function(xml) {
    ## Original line break (?).
    "\n"
  },

  code = function(xml) {
    list("\\code{", xml_contents(xml), "}")
  },

  inline_html = function(xml) {
    ## Will just ignore this for now.
    xml_contents(xml)
  },

  emph = function(xml) {
    ## Emphasized text.
    list("\\emph{", xml_contents(xml), "}")
  },

  strong = function(xml) {
    ## Bold text.
    list("\\strong{", xml_contents(xml), "}")
  },

  link = function(xml) {
    ## Hyperlink, this can also be a link to a function
    dest <- xml_attr(xml, "destination")
    contents <- xml_contents(xml)

    link <- parse_link(dest, contents)

    if (!is.null(link)) {
      link
    } else if (dest == "") {
      list("\\url{", xml_contents(xml), "}")
    } else {
      list("\\href{", dest, "}{", xml_contents(xml), "}")
    }
  },

  image = function(xml) {
    dest <- xml_attr(xml, "destination")
    title <- xml_attr(xml, "title")
    paste0("\\figure{", dest, "}{", title, "}")
  }

)

markdown_tags_restricted <- markdown_tags

ws_to_empty <- function(x) {
  sub("^\\s*$", "", x)
}

## If the string contains \preformatted, then
## we prefix all leading whitespace with a random string.
## This is needed because the commonmark parser eats up
## all leading whitespace and we want to keep the whitespace
## within \preformatted{}. We actually keep leading whitespace
## everywhere, but that is OK, it is ignored in Rd.

md_leading_ws_string <- "c72663012cdc3c9e55cb3f4e1b4a4df9"

escape_leading_whitespace <- function(text) {
  if (grepl("\\preformatted", text)) {
    text <- gsub(
      pattern = "(?m)^(\\s+)",
      replacement = paste0(md_leading_ws_string, "\\1"),
      x = text,
      perl = TRUE
    )
  }
  text
}

unescape_leading_whitespace <- function(text) {
  if (grepl(md_leading_ws_string, text)) {
    text <- gsub(
      pattern = md_leading_ws_string,
      replacement = "",
      x = text,
      fixed = TRUE
    )
  }
  text
}

## Parse a MarkDown link, to see if we should create an Rd link
##
## * [](::function) becomes \code{\link{function}}
## * [](pkg::function) becomes \code{\link[pkg]{function}}
## * [name](::=dest) becomes \link[=dest]{name}
## * [name](pkg::bar) becomes \link[pkg:bar]{name}
##
## S3/S4 classes are also supported
## * [terms](::=terms.object) becomes \link[=terms.object]{terms}
## * [abc](::=abc-class) becomes \link[=abc-class]{abc}
##
## These actually do not require any special treatment from the
## parser, they are included in the four cases above.

parse_link <- function(destination, contents) {

  ## destination must contain :: exactly once, otherwise it is not an Rd link
  if (!contains_once(destination, "::", fixed = TRUE)) return(NULL)

  ## if contents is a `code tag`, then we need to move this outside
  ## of the link, Rd does not like \link{\code{}}, only \code{\link{}}
  pre <- post <- ""
  if (length(contents) == 3 &&
      xml_name(contents[[1]]) == "text" &&
      xml_name(contents[[2]]) == "code" &&
      xml_name(contents[[3]]) == "text") {
    pre <- paste0(ws_to_empty(xml_text(contents[[1]])), "\\code{")
    post <- paste0("}", ws_to_empty(xml_text(contents[[3]])))
    contents <- xml_contents(contents[[2]])
  }

  split_contents <- strsplit(destination, "::", fixed = TRUE)[[1]]
  pkg <- split_contents[1]
  func <- split_contents[2]

  if (is_empty_xml(contents) && pkg == "") {
    ## [](::function) -> \code{\link{function}}
    paste0("\\code{\\link{", func, "}}")

  } else if (is_empty_xml(contents)) {
    ## [](pkg::function) -> \code{\link[pkg]{function}}
    paste0("\\code{\\link[", pkg, "]{", func, "}}")

  } else if (pkg == "") {
    ## [name](::=dest) -> \link[=dest]{name}
    list(pre, paste0("\\link[", func, "]{"), contents, "}", post)

  } else {
    ## [name](pkg::bar) -> \link[pkg:bar]{name}
    list(pre, paste0("\\link[", pkg, ":", func, "]{"), contents, "}", post)
  }
}

## Check if a string contains a pattern exactly once. Overlapping
## patterns do not count.

contains_once <- function(x, pattern, ...) {
  length(strsplit(x = x, split = pattern, ...)[[1]]) == 2
}

is_empty_xml <- function(x) {
  is(x, "xml_nodeset") && length(x) == 0
}
