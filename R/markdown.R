markdown <- function(text, tag = NULL, sections = FALSE) {
  expanded_text <- tryCatch(
    markdown_evaluate(text),
    error = function(e) {
      warn_roxy_tag(tag, "failed to evaluate inline markdown code", parent = e)
      text
    }
  )
  tokens <- md_tokenize(expanded_text, tag)
  tryCatch(
    markdown_pass2(tokens, tag = tag, sections = sections),
    error = function(e) {
      warn_roxy_tag(tag, "markdown failed to process", parent = e)
      text
    }
  )
}

markdown_pass2 <- function(tokens, tag = NULL, sections = FALSE) {
  text_linkrefs <- add_linkrefs_to_md(tokens$text)

  xml <- commonmark::markdown_xml(
    text_linkrefs,
    hardbreaks = TRUE,
    extensions = "table"
  )

  state <- new.env(parent = emptyenv())
  state$tag <- tag
  state$has_sections <- sections
  state$tokens <- tokens$tokens
  state$types <- tokens$types
  mdxml_children_to_rd_top(xml, state)
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

# The XML -> Rd tree walk happens in C++ (src/mdxmlToRd.cpp); everything
# that needs package state -- link resolution, R code detection, and
# warnings -- is supplied as a callback
mdxml_children_to_rd_top <- function(xml, state) {
  state$section_tag <- uuid()
  result <- mdxmlToRd(
    xml,
    tokens = state$tokens %||% character(),
    types = state$types %||% character(),
    has_sections = isTRUE(state$has_sections),
    section_tag = state$section_tag,
    restrict_images = roxy_meta_get("restrict_image_formats") %||% TRUE,
    resolve_link = function(destination, text, has_nontext, is_code, rendered) {
      parse_link(destination, text, has_nontext, is_code, rendered, state)
    },
    is_r_code = function(code) can_parse(code) || code %in% special,
    warn = function(kind, detail) mdxml_warn(kind, detail, state)
  )

  rd <- result$rd
  if (state$has_sections) {
    secs <- strsplit(rd, state$section_tag, fixed = TRUE)[[1]] %||% ""
    titles <- c("", result$titles)
    # strsplit drops trailing empty strings, so pad to match titles length
    secs <- c(secs, rep("", length(titles) - length(secs)))
    rd <- structure(trimws(secs), names = titles)
  }
  rd
}

mdxml_warn <- function(kind, detail, state) {
  if (kind == "unsupported") {
    warn_roxy_tag(
      state$tag,
      c(
        "markdown translation failed",
        x = "{detail} are not currently supported"
      )
    )
  } else if (kind == "heading") {
    if (is.null(state$tag)) {
      tag_name <- "this tag"
    } else {
      tag_name <- paste0("@", state$tag$tag)
    }
    warn_roxy_tag(
      state$tag,
      c(
        "markdown translation failed",
        x = "Level 1 headings are not supported in {tag_name}",
        i = "Do you want to put the heading in @description or @details?"
      )
    )
  } else {
    warn_roxy_tag(
      state$tag,
      c(
        "markdown translation failed",
        x = "Internal error: unknown xml node {detail}",
        i = "Please file an issue at https://github.com/r-lib/roxygen2/issues"
      )
    )
  }
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
