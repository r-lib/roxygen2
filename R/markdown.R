markdown <- function(text, tag = NULL, sections = FALSE) {
  esc_text <- escape_rd_for_md(text)
  esc_text_linkrefs <- add_linkrefs_to_md(esc_text)

  mdxml <- md_to_mdxml(esc_text_linkrefs)
  state <- new.env(parent = emptyenv())
  state$tag <- tag
  state$has_sections <- sections
  rd <- mdxml_children_to_rd_top(mdxml, state)

  map_chr(rd, unescape_rd_for_md, esc_text)
}

md_to_mdxml <- function(x) {
  md <- commonmark::markdown_xml(x, hardbreaks = TRUE)
  xml2::read_xml(md)
}

mdxml_children_to_rd_top <- function(xml, state) {
  state$section_tag <- uuid()
  out <- map_chr(xml_children(xml), mdxml_node_to_rd, state)
  out <- c(out, mdxml_close_sections(state))
  rd <- paste0(out, collapse = "")
  secs <- strsplit(rd, state$section_tag, fixed = TRUE)[[1]]
  if (length(secs) == 0) secs <- ""
  str_trim(secs)
}

mdxml_children_to_rd <- function(xml, state) {
  out <- map_chr(xml_children(xml), mdxml_node_to_rd, state)
  paste0(out, collapse = "")
}

#' @importFrom xml2 xml_name xml_type xml_text xml_contents xml_attr xml_children
mdxml_node_to_rd <- function(xml, state) {
  if (!inherits(xml, "xml_node") || xml_type(xml) != "element") {
    roxy_tag_warning(state$tag, "Internal markdown translation failure")
    return("")
  }

  switch(xml_name(xml),
    html = ,
    document = ,
    unknown = mdxml_children_to_rd(xml, state),

    paragraph = paste0("\n\n", mdxml_children_to_rd(xml, state)),
    text = escape_comment(xml_text(xml)),
    emph = paste0("\\emph{", mdxml_children_to_rd(xml, state), "}"),
    strong = paste0("\\strong{", mdxml_children_to_rd(xml, state), "}"),
    softbreak = "\n",
    linebreak = "\n",

    code = mdxml_code(xml, state),
    code_block = paste0("\\preformatted{", escape_verb(xml_text(xml)), "}"),

    list = mdxml_list(xml, state),
    item = mdxml_item(xml, state),
    link = mdxml_link(xml),
    image = mdxml_image(xml),

    # Only supported when including Rmds
    heading = mdxml_heading(xml, state),

    # Not supported
    block_quote = mdxml_unsupported(xml, state$tag, "block quotes"),
    hrule = mdxml_unsupported(xml, state$tag, "horizontal rules"),
    html_inline = mdxml_unsupported(xml, state$tag, "inline HTML"),
    mdxml_unknown(xml, state$tag)
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

mdxml_code <- function(xml, tag) {
  code <- xml_text(xml)

  # See escaping details at
  # https://cran.rstudio.com/doc/manuals/r-devel/R-exts.html#Insertions
  if (can_parse(code)) {
    paste0("\\code{", gsub("%", "\\\\%", code), "}")
  } else {
    paste0("\\verb{", escape_verb(code), "}")
  }
}

can_parse <- function(x) {
  tryCatch({
    parse_expr(x)
    TRUE
  }, error = function(x) FALSE)
}

escape_verb <- function(x) {
  # Don't need to escape \\ because that's already handled in double_escape_md()
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("{", "\\{", x, fixed = TRUE)
  x <- gsub("}", "\\}", x, fixed = TRUE)
  x
}

# A list, either bulleted or numbered
mdxml_list <- function(xml, state) {
  type <- xml_attr(xml, "type")
  if (type == "ordered") {
    paste0("\n\\enumerate{", mdxml_children_to_rd(xml, state), "\n}")
  } else {
    paste0("\n\\itemize{", mdxml_children_to_rd(xml, state), "\n}")
  }
}

mdxml_item <- function(xml, state) {
  ## A single item within a list. We remove the first paragraph
  ## tag, to avoid an empty line at the beginning of the first item.
  children <- xml_children(xml)
  if (length(children) == 0) {
    cnts <- ""
  } else if (xml_name(children[[1]]) == "paragraph") {
    cnts <- paste0(
      mdxml_children_to_rd(children[[1]], state),
      paste0(map_chr(children[-1], mdxml_node_to_rd, state), collapse = "")
    )
  } else {
    cnts <- mdxml_children_to_rd(xml, state)
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
  gsub("%", "\\%", x, fixed = TRUE)
}

mdxml_heading <- function(xml, state) {
  level <- xml_attr(xml, "level")
  if (! state$has_sections && level == 1) {
    return(mdxml_unsupported(xml, state$tag, "level 1 markdown headings"))
  }
  head <- paste0(
    mdxml_close_sections(state, level),
    "\n",
    if (level == 1) paste0(state$section_tag, "\\section{"),
    if (level > 1) "\\subsection{",
    xml_text(xml),
    "}{")
  state$section <- c(state$section, level)
  head
}

#' @importFrom utils head tail

mdxml_close_sections <- function(state, upto = 1L) {
  hmy <- 0L
  while (length(state$section) && tail(state$section, 1) >= upto) {
    hmy <- hmy + 1L
    state$section <- head(state$section, -1L)
  }

  paste0(rep("\n}\n", hmy), collapse = "")
}
