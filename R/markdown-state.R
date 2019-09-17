
## If not specified in DESCRIPTION
markdown_global_default <- FALSE

## Hacky global switch - this uses the fact that blocks are parsed
## one after the another, and that we set markdown on/off before each
## block

markdown_env <- new.env(parent = emptyenv())
markdown_on <- function(value = NULL) {
  if (!is.null(value)) {
    assign("markdown-support", isTRUE(value), envir = markdown_env)
  }
  return(isTRUE(markdown_env$`markdown-support`))
}

markdown_activate <- function(tags, global_options = list()) {
  ## markdown on/off based on global flag and presence of @md & @nomd
  ## we need to use markdown_global_default as well, because global_options
  ## can be NULL, e.g. if called from parse_text()

  names <- purrr::map_chr(tags, "tag")
  has_md <- "md" %in% names
  has_nomd <- "noMd" %in% names

  if (has_md && has_nomd) {
    roxy_tag_warning(tags[[1]], "Both @md and @noMd, no markdown parsing")
  }

  md <- global_options$markdown %||% markdown_global_default
  if (has_md) md <- TRUE
  if (has_nomd) md <- FALSE

  markdown_on(md)
}
