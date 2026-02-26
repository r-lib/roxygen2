markdown <- function(text, tag = NULL, sections = FALSE) {
  tag <- tag %||% list(file = NA, line = NA)
  expanded_text <- tryCatch(
    markdown_pass1(text),
    error = function(e) {
      warn_roxy_tag(tag, "failed to evaluate inline markdown code", parent = e)
      text
    }
  )
  escaped_text <- escape_rd_for_md(expanded_text)
  tryCatch(
    markdown_pass2(escaped_text, tag = tag, sections = sections),
    error = function(e) {
      warn_roxy_tag(tag, "markdown failed to process", parent = e)
      text
    }
  )
}

mddata <- new.env(parent = emptyenv())

#' Expand the embedded inline code
#'
#' @details
#' For example this becomes two: `r 1+1`.
#' Variables can be set and then reused, within the same
#' tag: `r x <- 100; NULL`
#' The value of `x` is `r x`.
#'
#' We have access to the internal functions of the package, e.g.
#' since this is _roxygen2_, we can refer to the internal `markdown`
#' function, and this is `TRUE`: `r is.function(markdown)`.
#'
#' To insert the name of the current package: `r packageName()`.
#'
#' The `iris` data set has `r ncol(iris)` columns:
#' `r paste0("\x60\x60", colnames(iris), "\x60\x60", collapse = ", ")`.
#'
#' ```{r}
#' # Code block demo
#' x + 1
#' ```
#'
#' Chunk options:
#'
#' ```{r results = "hold"}
#' names(mtcars)
#' nrow(mtcars)
#' ```
#'
#' Plots:
#'
#' ```{r test-figure}
#' plot(1:10)
#' ```
#'
#' Alternative knitr engines:
#'
#' ```{verbatim}
#' #| file = "tests/testthat/example.Rmd"
#' ```
#'
#' Also see `vignette("rd-formatting")`.
#'
#' @param text Input text.
#' @return
#' Text with R code expanded.
#' A character vector of the same length as the input `text`.
#'
#' @keywords internal

markdown_pass1 <- function(text) {
  rm(list = ls(envir = mddata), envir = mddata)
  text <- paste(text, collapse = "\n")
  mdxml <- xml_ns_strip(md_to_mdxml(text, sourcepos = TRUE))
  code_nodes <- xml_find_all(mdxml, ".//code | .//code_block")
  rcode_nodes <- keep(code_nodes, is_markdown_code_node)
  if (length(rcode_nodes) == 0) {
    return(text)
  }
  rcode_pos <- parse_md_pos(map_chr(rcode_nodes, xml_attr, "sourcepos"))
  rcode_pos <- work_around_cmark_sourcepos_bug(text, rcode_pos)
  out <- eval_code_nodes(rcode_nodes)
  str_set_all_pos(text, rcode_pos, out, rcode_nodes)
}

# Work around commonmark sourcepos bug for inline R code
# https://github.com/r-lib/roxygen2/issues/1353
work_around_cmark_sourcepos_bug <- function(text, rcode_pos) {
  if (Sys.getenv("ROXYGEN2_NO_SOURCEPOS_WORKAROUND", "") != "") {
    return(rcode_pos)
  }

  lines <- str_split(text, fixed("\n"))[[1]]

  for (l in seq_len(nrow(rcode_pos))) {
    # Do not try to fix multi-line code, we error for that (below)
    if (rcode_pos$start_line[l] != rcode_pos$end_line[l]) {
      next
    }
    line <- lines[rcode_pos$start_line[l]]
    start <- rcode_pos$start_column[l]

    # Maybe correct? At some point this will be fixed upstream, hopefully.
    if (str_sub(line, start - 1, start + 1) == "`r ") {
      next
    }

    # Maybe indented and we can shift it?
    # It is possible that the shift that we try accidentally matches
    # "`r ", but it seems to be extremely unlikely. An example is this:
    # #'       ``1`r `` `r 22*10`
    # (seven spaces after the #', so an indent of six spaces. If we shift
    # the real "`r " left by six characters, there happens to be another
    # "`r " there.

    indent <- nchar(str_extract(line, "^[ ]+"))
    if (str_sub(line, start - 1 + indent, start + 1 + indent) == "`r ") {
      rcode_pos$start_column[l] <- rcode_pos$start_column[l] + indent
      rcode_pos$end_column[l] <- rcode_pos$end_column[l] + indent
    }
  }

  rcode_pos
}

is_markdown_code_node <- function(x) {
  info <- xml_attr(x, "info")
  str_sub(xml_text(x), 1, 2) == "r " ||
    (!is.na(info) && grepl("^[{][a-zA-z]+[}, ]", info))
}

parse_md_pos <- function(text) {
  nums <- map(strsplit(text, "[:-]"), as.integer)
  data.frame(
    start_line = map_int(nums, 1),
    start_column = map_int(nums, 2),
    end_line = map_int(nums, 3),
    end_column = map_int(nums, 4)
  )
}

eval_code_nodes <- function(nodes) {
  evalenv <- roxy_meta_get("evalenv")
  # This should only happen in our test cases
  if (is.null(evalenv)) {
    evalenv <- new.env(parent = baseenv())
  }

  map_chr(nodes, eval_code_node, env = evalenv)
}

eval_code_node <- function(node, env) {
  if (xml_name(node) == "code") {
    # write knitr markup for inline code
    text <- paste0("`", xml_text(node), "`")
  } else {
    lang <- xml_attr(node, "info")
    # write knitr markup for fenced code
    text <- paste0("```", if (!is.na(lang)) lang, "\n", xml_text(node), "```\n")
  }

  chunk_opts <- utils::modifyList(
    knitr_chunk_defaults(),
    as.list(roxy_meta_get("knitr_chunk_options", NULL))
  )

  roxy_knit(text, env, chunk_opts)
}

knitr_chunk_defaults <- function() {
  list(
    error = FALSE,
    fig.path = "man/figures/",
    fig.process = basename,
    comment = "#>",
    collapse = TRUE
  )
}

str_set_all_pos <- function(text, pos, value, nodes) {
  # Cmark has a bug when reporting source positions for multi-line
  # code tags, and it does not count the indenting space in the
  # continuation lines: https://github.com/commonmark/cmark/issues/296
  types <- xml_name(nodes)
  if (any(types == "code" & pos$start_line != pos$end_line)) {
    cli::cli_abort("multi-line `r ` markup is not supported", call = NULL)
  }

  # Need to split the string, because of the potential multi-line
  # code tags, and then also recode the positions
  lens <- nchar(str_split(text, fixed("\n"))[[1]])
  shifts <- c(0, cumsum(lens + 1L))
  shifts <- shifts[-length(shifts)]
  start <- shifts[pos$start_line] + pos$start_column
  end <- shifts[pos$end_line] + pos$end_column

  # Create intervals for the parts we keep
  keep_start <- c(1, end + 2L)
  keep_end <- c(start - 2L, nchar(text))

  # Now piece them together
  out <- paste0(
    substring(text, keep_start, keep_end),
    c(value, ""),
    collapse = ""
  )
  attributes(out) <- attributes(text)
  out
}

markdown_pass2 <- function(text, tag = NULL, sections = FALSE) {
  esc_text_linkrefs <- add_linkrefs_to_md(text)

  mdxml <- md_to_mdxml(esc_text_linkrefs)
  state <- new.env(parent = emptyenv())
  state$tag <- tag
  state$has_sections <- sections
  rd <- mdxml_children_to_rd_top(mdxml, state)

  map_chr(rd, unescape_rd_for_md, text)
}

md_to_mdxml <- function(x, ...) {
  md <- commonmark::markdown_xml(
    x,
    hardbreaks = TRUE,
    extensions = "table",
    ...
  )
  xml2::read_xml(md)
}

mdxml_children_to_rd_top <- function(xml, state) {
  state$section_tag <- uuid()
  out <- map_chr(xml_children(xml), mdxml_node_to_rd, state)
  out <- c(out, mdxml_close_sections(state))
  rd <- str_trim(paste0(out, collapse = ""))
  if (state$has_sections) {
    secs <- strsplit(rd, state$section_tag, fixed = TRUE)[[1]] %||% ""
    titles <- c("", state$titles)
    rd <- structure(str_trim(secs), names = titles)
  }
  rd
}

mdxml_children_to_rd <- function(xml, state) {
  out <- map_chr(xml_children(xml), mdxml_node_to_rd, state)
  paste0(out, collapse = "")
}

mdxml_node_to_rd <- function(xml, state) {
  if (
    !inherits(xml, "xml_node") ||
      !xml_type(xml) %in% c("text", "element")
  ) {
    warn_roxy_tag(
      state$tag,
      c(
        "markdown translation failed",
        x = "Unexpected internal error",
        i = "Please file an issue at https://github.com/r-lib/roxygen2/issues"
      )
    )
    return("")
  }

  switch(
    xml_name(xml),
    html = ,
    document = ,
    unknown = mdxml_children_to_rd(xml, state),

    paragraph = paste0("\n\n", mdxml_children_to_rd(xml, state)),
    text = if (is_true(state$in_link_code)) {
      escape_verb(xml_text(xml))
    } else {
      escape_comment(xml_text(xml))
    },
    emph = paste0("\\emph{", mdxml_children_to_rd(xml, state), "}"),
    strong = paste0("\\strong{", mdxml_children_to_rd(xml, state), "}"),
    softbreak = mdxml_break(state),
    linebreak = mdxml_break(state),

    code = mdxml_code(xml, state),
    code_block = mdxml_code_block(xml, state),

    table = mdxml_table(xml, state),
    list = mdxml_list(xml, state),
    item = mdxml_item(xml, state),
    link = mdxml_link(xml, state),
    image = mdxml_image(xml),
    heading = mdxml_heading(xml, state),

    # Only supported when including Rmds
    html_block = mdxml_html_block(xml, state),
    html_inline = mdxml_html_inline(xml, state),

    # Not supported
    block_quote = mdxml_unsupported(xml, state$tag, "block quotes"),
    hrule = mdxml_unsupported(xml, state$tag, "horizontal rules"),
    mdxml_unknown(xml, state$tag)
  )
}

mdxml_unknown <- function(xml, tag) {
  warn_roxy_tag(
    tag,
    c(
      "markdown translation failed",
      x = "Internal error: unknown xml node {xml_name(xml)}",
      i = "Please file an issue at https://github.com/r-lib/roxygen2/issues"
    )
  )
  escape_comment(xml_text(xml))
}
mdxml_unsupported <- function(xml, tag, feature) {
  warn_roxy_tag(
    tag,
    c(
      "markdown translation failed",
      x = "{feature} are not currently supported"
    )
  )
  escape_comment(xml_text(xml))
}

mdxml_break <- function(state) {
  if (isTRUE(state$inlink)) " " else "\n"
}

mdxml_code <- function(xml, tag) {
  code <- xml_text(xml)

  # See escaping details at
  # https://cran.rstudio.com/doc/manuals/r-devel/R-exts.html#Insertions
  if (can_parse(code) || code %in% special) {
    paste0("\\code{", gsub("%", "\\\\%", code), "}")
  } else {
    paste0("\\verb{", escape_verb(code), "}")
  }
}

special <- c(
  "-",
  ":",
  "::",
  ":::",
  "!",
  "!=",
  "(",
  "[",
  "[[",
  "@",
  "*",
  "/",
  "&",
  "&&",
  "%*%",
  "%/%",
  "%%",
  "%in%",
  "%o%",
  "%x%",
  "^",
  "+",
  "<",
  "<=",
  "=",
  "==",
  ">",
  ">=",
  "|",
  "||",
  "~",
  "$",
  "for",
  "function",
  "if",
  "repeat",
  "while"
)

mdxml_code_block <- function(xml, state) {
  info <- xml_attr(xml, "info", default = "")[1]
  if (nchar(info[1]) == 0) {
    info <- NA_character_
  }
  paste0(
    "\n\n",
    "\\if{html}{\\out{<div class=\"sourceCode",
    if (!is.na(info)) paste0(" ", info),
    "\">}}",
    "\\preformatted{",
    escape_verb(xml_text(xml)),
    "}",
    "\\if{html}{\\out{</div>}}"
  )
}

can_parse <- function(x) {
  tryCatch(
    {
      parse_expr(x)
      TRUE
    },
    error = function(x) FALSE
  )
}

escape_verb <- function(x) {
  # Don't need to escape \\ because that's already handled in double_escape_md()
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("{", "\\{", x, fixed = TRUE)
  x <- gsub("}", "\\}", x, fixed = TRUE)
  x
}

mdxml_table <- function(xml, state) {
  head <- xml_children(xml)[[1]]
  align <- substr(xml_attr(xml_children(head), "align", default = "left"), 1, 1)

  rows <- xml_find_all(xml, "d1:table_row|d1:table_header")
  cells <- map(rows, xml_find_all, "d1:table_cell")

  cells_rd <- map(cells, \(x) map(x, mdxml_children_to_rd, state = state))
  rows_rd <- map_chr(cells_rd, paste0, collapse = " \\tab ")

  paste0(
    "\\tabular{",
    paste(align, collapse = ""),
    "}{\n",
    paste("  ", rows_rd, "\\cr\n", collapse = ""),
    "}\n"
  )
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

mdxml_link <- function(xml, state) {
  ## Hyperlink, this can also be a link to a function
  dest <- xml_attr(xml, "destination")
  contents <- xml_contents(xml)

  link <- parse_link(dest, contents, state)

  if (!is.null(link)) {
    paste0(link, collapse = "")
  } else if (dest == "" || dest == xml_text(xml)) {
    paste0("\\url{", escape_comment(xml_text(xml)), "}")
  } else {
    paste0(
      "\\href{",
      escape_comment(dest),
      "}",
      "{",
      mdxml_link_text(contents, state),
      "}"
    )
  }
}

mdxml_link_text <- function(xml_contents, state) {
  # Newlines in markdown get converted to softbreaks/linebreaks by
  # markdown_xml(), which then get interpreted as empty strings by
  # xml_text(). So we preserve newlines as spaces.
  inlink <- state$inlink
  on.exit(state$inlink <- inlink, add = TRUE)
  state$inlink <- TRUE

  text <- map_chr(xml_contents, mdxml_node_to_rd, state)
  paste0(text, collapse = "")
}

mdxml_image <- function(xml) {
  dest <- xml_attr(xml, "destination")
  title <- xml_attr(xml, "title")
  fmt <- get_image_format(dest)
  paste0(
    if (fmt == "html") "\\if{html}{",
    if (fmt == "pdf") "\\if{pdf}{",
    "\\figure{",
    dest,
    "}",
    if (nchar(title)) paste0("{", title, "}"),
    if (fmt %in% c("html", "pdf")) "}"
  )
}

get_image_format <- function(path) {
  should_restrict <- roxy_meta_get("restrict_image_formats") %||% TRUE
  if (!should_restrict) {
    return("all")
  }

  path <- tolower(path)
  rx <- default_image_formats()
  html <- grepl(rx$html, path)
  pdf <- grepl(rx$pdf, path)
  if (html && pdf) {
    "all"
  } else if (html) {
    "html"
  } else if (pdf) {
    "pdf"
  } else {
    "all"
  }
}

default_image_formats <- function() {
  list(
    html = "[.](jpg|jpeg|gif|png|svg)$",
    pdf = "[.](jpg|jpeg|gif|png|pdf)$"
  )
}

escape_comment <- function(x) {
  gsub("%", "\\%", x, fixed = TRUE)
}

mdxml_heading <- function(xml, state) {
  level <- xml_attr(xml, "level")
  if (!state$has_sections && level == 1) {
    warn_roxy_tag(
      state$tag,
      c(
        "markdown translation failed",
        x = "Level 1 headings are not supported in @{state$tag$tag}",
        i = "Do you want to put the heading in @description or @details?"
      )
    )
    return(escape_comment(xml_text(xml)))
  }

  txt <- map_chr(xml_contents(xml), mdxml_node_to_rd, state)
  if (level == 1) {
    state$titles <- c(state$titles, paste(txt, collapse = ""))
  }
  head <- paste0(
    mdxml_close_sections(state, level),
    "\n",
    if (level == 1) state$section_tag else "\\subsection{",
    if (level > 1) paste(txt, collapse = ""),
    if (level > 1) "}{"
  )
  state$section <- c(state$section, level)
  head
}

mdxml_html_block <- function(xml, state) {
  txt <- xml_text(xml)
  txt <- gsub("}", "\\}", txt, fixed = TRUE)
  txt <- gsub("{", "\\{", txt, fixed = TRUE)
  paste0(
    "\\if{html}{\\out{\n",
    txt,
    "}}\n"
  )
}

mdxml_html_inline <- function(xml, state) {
  paste0(
    "\\if{html}{\\out{",
    gsub("}", "\\}", xml_text(xml), fixed = TRUE),
    "}}"
  )
}

mdxml_close_sections <- function(state, upto = 1L) {
  hmy <- 0L
  upto <- max(upto, 2L)
  while (length(state$section) && tail(state$section, 1) >= upto) {
    hmy <- hmy + 1L
    state$section <- head(state$section, -1L)
  }

  paste0(rep("\n}\n", hmy), collapse = "")
}
