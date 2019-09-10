markdown_if_active <- function(text, tag) {
  if (markdown_on()) {
    markdown(text, tag)
  } else {
    text
  }
}

markdown <- function(text, tag = NULL) {
  esc_text <- escape_rd_for_md(text)
  esc_text_linkrefs <- add_linkrefs_to_md(esc_text)

  mdxml <- md_to_mdxml(esc_text_linkrefs)
  rd <- mdxml_children_to_rd(mdxml, tag)

  unescape_rd_for_md(str_trim(rd), esc_text)
}

md_to_mdxml <- function(x) {
  md <- commonmark::markdown_xml(x, hardbreaks = TRUE)
  xml2::read_xml(md)
}

mdxml_children_to_rd <- function(xml, tag) {
  out <- map_chr(xml_children(xml), mdxml_node_to_rd, tag)
  paste0(out, collapse = "")
}

#' @importFrom xml2 xml_name xml_type xml_text xml_contents xml_attr xml_children
mdxml_node_to_rd <- function(xml, tag) {
  if (!inherits(xml, "xml_node") || xml_type(xml) != "element") {
    roxy_tag_warning(tag, "Internal markdown translation failure")
    return("")
  }

  switch(xml_name(xml),
    html = ,
    document = ,
    unknown = mdxml_children_to_rd(xml, tag),

    code_block = paste0("\\preformatted{", gsub("%", "\\\\%", xml_text(xml)), "}"),
    paragraph = paste0("\n\n", mdxml_children_to_rd(xml, tag)),
    text = escape_comment(xml_text(xml)),
    code = paste0("\\code{", gsub("%", "\\\\%", xml_text(xml)), "}"),
    emph = paste0("\\emph{", mdxml_children_to_rd(xml, tag), "}"),
    strong = paste0("\\strong{", mdxml_children_to_rd(xml, tag), "}"),
    softbreak = "\n",
    linebreak = "\n",

    list = mdxml_list(xml, tag),
    item = mdxml_item(xml, tag),
    link = mdxml_link(xml),
    image = mdxml_image(xml),

    # Not supported
    heading = mdxml_unsupported(xml, tag, "markdown headings"),
    block_quote = mdxml_unsupported(xml, tag, "block quotes"),
    hrule = mdxml_unsupported(xml, tag, "horizontal rules"),
    html_inline = mdxml_unsupported(xml, tag, "inline HTML"),
    mdxml_unknown(xml, tag)
  )
}

mdxml_unknown <- function(xml, tag) {
  roxy_tag_warning(tag, "Unknown xml node: ", xml_name(xml))
  escape_comment(xml_text(xml))
}
mdxml_unsupported <- function(xml, tag, feature) {
  roxy_tag_warning(tag, "Use of ", feature, " is not currently supported")
  escape_comment(xml_text(xml))
}


# A list, either bulleted or numbered
mdxml_list <- function(xml, tag) {
  type <- xml_attr(xml, "type")
  if (type == "ordered") {
    paste0("\n\\enumerate{", mdxml_children_to_rd(xml, tag), "\n}")
  } else {
    paste0("\n\\itemize{", mdxml_children_to_rd(xml, tag), "\n}")
  }
}

mdxml_item <- function(xml, tag) {
  ## A single item within a list. We remove the first paragraph
  ## tag, to avoid an empty line at the beginning of the first item.
  children <- xml_children(xml)
  if (length(children) == 0) {
    cnts <- ""
  } else if (xml_name(children[[1]]) == "paragraph") {
    cnts <- paste0(
      mdxml_children_to_rd(children[[1]], tag),
      paste0(map_chr(children[-1], mdxml_node_to_rd, tag), collapse = "")
    )
  } else {
    cnts <- mdxml_children_to_rd(xml, tag)
  }
  paste0("\n\\item ", cnts)
}

mdxml_link <- function(xml) {
  ## Hyperlink, this can also be a link to a function
  dest <- xml_attr(xml, "destination")
  contents <- xml_contents(xml)

  link <- parse_link(dest, contents)

  if (!is.null(link)) {
    paste0(link, collapse = "")
  } else if (dest == "" || dest == xml_text(xml)) {
    paste0("\\url{", escape_comment(xml_text(xml)), "}")
  } else {
    paste0("\\href{", dest, "}{", mdxml_link_text(contents), "}")
  }
}

# Newlines in markdown get converted to softbreaks/linebreaks by
# markdown_xml(), which then get interpreted as empty strings by
# xml_text(). So we preserve newlines as spaces.
mdxml_link_text <- function(xml_contents) {
  text <- xml_text(xml_contents)
  text[xml_name(xml_contents) %in% c("linebreak", "softbreak")] <- " "
  escape_comment(paste0(text, collapse = ""))
}

mdxml_image = function(xml) {
  dest <- xml_attr(xml, "destination")
  title <- xml_attr(xml, "title")
  paste0("\\figure{", dest, "}{", title, "}")
}


escape_comment <- function(x) {
  # Strip existing escaping - this is admittedly a hack and makes the
  # state of % escaping more confusing, but it only affects markdown, and
  # I think if you do want a literal \% you can do `\%`
  x <- gsub("\\%", "%", x, fixed = TRUE)
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x
}
