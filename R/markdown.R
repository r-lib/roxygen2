markdown <- function(text) {
  if (!markdown_on()) {
    return(text)
  }

  esc_text <- escape_rd_for_md(text)
  esc_text_linkrefs <- add_linkrefs_to_md(esc_text)

  mdxml <- md_to_mdxml(esc_text_linkrefs)
  rd <- mdxml_to_rd(mdxml)

  unescape_rd_for_md(str_trim(rd), esc_text)
}

md_to_mdxml <- function(x) {
  md <- commonmark::markdown_xml(x, hardbreaks = TRUE)
  xml2::read_xml(md)
}

#' @importFrom xml2 xml_name xml_type xml_text xml_contents xml_attr xml_children
mdxml_to_rd <- function(xml) {
  if (is.character(xml)) {
    # base case: string
    paste(xml, collapse = "")
  } else if (inherits(xml, "xml_node") && xml_type(xml) == "text") {
    # base case: text node, only keep if not entirely whitespace
    text <- xml_text(xml)
    sub("^\\s*$", "", text)
  } else if (inherits(xml, "xml_node") && xml_type(xml) == "element") {
    # recursive case: generic node
    parser <- markdown_tags[[xml_name(xml)]]

    if (is.null(parser)) {
      warning("Unknown xml node: ", xml_name(xml), call. = FALSE)
      return(xml_text(xml))
    }

    parser(xml)
  } else if (is.list(xml) || inherits(xml, "xml_nodeset")) {
    # recursive case: list or nodeset
    paste(vapply(xml, mdxml_to_rd, character(1)), collapse = "")
  } else {
    warning("Unknown xml object")
  }
}

markdown_tags <- list(

  unknown = function(xml) {
    ## Just ignore the tag, keep the contents
    mdxml_to_rd(xml_contents(xml))
  },

  document = function(xml) {
    ## Keep the contents only, tag does nothing
    mdxml_to_rd(xml_contents(xml))
  },

  block_quote = function(xml) {
    ## Cannot really format this in Rd, so we just leave it
    mdxml_to_rd(xml_contents(xml))
  },

  list = function(xml) {
    ## A list, either bulleted or numbered
    type <- xml_attr(xml, "type")
    if (type == "ordered") {
      paste0("\n\\enumerate{", mdxml_to_rd(xml_contents(xml)), "\n}")
    } else {
      paste0("\n\\itemize{", mdxml_to_rd(xml_contents(xml)), "\n}")
    }
  },

  item = function(xml) {
    ## A single item within a list. We remove the first paragraph
    ## tag, to avoid an empty line at the beginning of the first item.
    cnts <- xml_children(xml)
    if (length(cnts) == 0) {
      cnts <- ""
    } else if (xml_name(cnts[[1]]) == "paragraph") {
      cnts <- paste0(mdxml_to_rd(xml_contents(cnts[[1]])), mdxml_to_rd(cnts[-1]))
    }
    paste0("\n\\item ", cnts)
  },

  code_block = function(xml) {
    ## Large block of code
    paste0("\\preformatted{", gsub("%", "\\\\%", xml_text(xml)), "}")
  },

  html = function(xml) {
    ## Not sure what to do with this, we'll just leave it
    mdxml_to_rd(xml_contents(xml))
  },

  paragraph = function(xml) {
    ## Paragraph
    paste0("\n\n", mdxml_to_rd(xml_contents(xml)))
  },

  header = function(xml) {
    ## Not sure what this is, ignore it for now.
    mdxml_to_rd(xml_contents(xml))
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
    paste0("\\code{", gsub("%", "\\\\%", xml_text(xml)), "}")
  },

  html_inline = function(xml) {
    ## Will just ignore this for now.
    mdxml_to_rd(xml_contents(xml))
  },

  emph = function(xml) {
    ## Emphasized text.
    paste0("\\emph{", mdxml_to_rd(xml_contents(xml)), "}")
  },

  strong = function(xml) {
    ## Bold text.
    paste0("\\strong{", mdxml_to_rd(xml_contents(xml)), "}")
  },

  link = function(xml) {
    ## Hyperlink, this can also be a link to a function
    dest <- xml_attr(xml, "destination")
    contents <- xml_contents(xml)

    link <- parse_link(dest, contents)

    if (!is.null(link)) {
      paste0(link, collapse = "")
    } else if (dest == "" || dest == xml_text(xml)) {
      paste0("\\url{", xml_text(xml), "}")
    } else {
      paste0("\\href{", dest, "}{", xml_link_text(contents), "}")
    }
  },

  image = function(xml) {
    dest <- xml_attr(xml, "destination")
    title <- xml_attr(xml, "title")
    paste0("\\figure{", dest, "}{", title, "}")
  }

)

# Newlines in markdown get converted to softbreaks/linebreaks by
# markdown_xml(), which then get interpreted as empty strings by
# xml_text(). So we preserve newlines as spaces.
xml_link_text <- function(xml_contents) {
  text <- xml_text(xml_contents)
  text[xml_name(xml_contents) %in% c("linebreak", "softbreak")] <- " "
  paste0(text, collapse = "")
}
