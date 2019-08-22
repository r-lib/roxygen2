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
  if (inherits(xml, "xml_node") && xml_type(xml) == "element") {
    # recursive case: generic node

    switch(xml_name(xml),
      html = ,
      document = ,
      unknown = mdxml_to_rd(xml_contents(xml)),

      code_block = paste0("\\preformatted{", gsub("%", "\\\\%", xml_text(xml)), "}"),
      paragraph = paste0("\n\n", mdxml_to_rd(xml_contents(xml))),
      text = xml_text(xml),
      code = paste0("\\code{", gsub("%", "\\\\%", xml_text(xml)), "}"),
      emph = paste0("\\emph{", mdxml_to_rd(xml_contents(xml)), "}"),
      strong = paste0("\\strong{", mdxml_to_rd(xml_contents(xml)), "}"),
      softbreak = "\n",
      linebreak = "\n",

      list = mdxml_list(xml),
      item = mdxml_item(xml),
      link = mxml_link(xml),
      image = mdxml_image(xml),

      # Not supported
      header = ,
      block_quote = ,
      hrule = ,
      html_inline = ,
      mdxml_unknown(xml)
    )
  } else if (inherits(xml, "xml_nodeset")) {
    # recursive case: list or nodeset
    paste(vapply(xml, mdxml_to_rd, character(1)), collapse = "")
  } else {
    warning("Internal failure in markdown translation.", call. = FALSE)
  }
}

mdxml_unknown <- function(xml) {
  warning("Unknown xml node: ", xml_name(xml), call. = FALSE)
  xml_text(xml)
}

# A list, either bulleted or numbered
mdxml_list <- function(xml) {
  type <- xml_attr(xml, "type")
  if (type == "ordered") {
    paste0("\n\\enumerate{", mdxml_to_rd(xml_contents(xml)), "\n}")
  } else {
    paste0("\n\\itemize{", mdxml_to_rd(xml_contents(xml)), "\n}")
  }
}

mdxml_item <- function(xml) {
  ## A single item within a list. We remove the first paragraph
  ## tag, to avoid an empty line at the beginning of the first item.
  cnts <- xml_children(xml)
  if (length(cnts) == 0) {
    cnts <- ""
  } else if (xml_name(cnts[[1]]) == "paragraph") {
    cnts <- paste0(mdxml_to_rd(xml_contents(cnts[[1]])), mdxml_to_rd(cnts[-1]))
  }
  paste0("\n\\item ", cnts)
}

mxml_link <- function(xml) {
  ## Hyperlink, this can also be a link to a function
  dest <- xml_attr(xml, "destination")
  contents <- xml_contents(xml)

  link <- parse_link(dest, contents)

  if (!is.null(link)) {
    paste0(link, collapse = "")
  } else if (dest == "" || dest == xml_text(xml)) {
    paste0("\\url{", xml_text(xml), "}")
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
  paste0(text, collapse = "")
}

mdxml_image = function(xml) {
  dest <- xml_attr(xml, "destination")
  title <- xml_attr(xml, "title")
  paste0("\\figure{", dest, "}{", title, "}")
}
