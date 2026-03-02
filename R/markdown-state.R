#' @export
roxy_tag_parse.roxy_tag_md <- function(x) tag_toggle(x)

#' @export
roxy_tag_parse.roxy_tag_noMd <- function(x) tag_toggle(x)

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

local_markdown <- function(env = parent.frame()) {
  old <- markdown_env$`markdown-support`
  markdown_on(TRUE)
  withr::defer(markdown_on(old), envir = env)
}

markdown_activate <- function(tags) {
  ## markdown on/off based on global flag and presence of @md & @nomd

  names <- purrr::map_chr(tags, "tag")
  has_md <- "md" %in% names
  has_nomd <- "noMd" %in% names

  if (has_md && has_nomd) {
    md_tag <- tags[names == "md"][[1]]
    warn_roxy_tag(md_tag, "conflicts with @noMd; turning markdown parsing off")

    md <- FALSE
  } else {
    md <- roxy_meta_get("markdown", FALSE)
    if (has_md) {
      md <- TRUE
    }
    if (has_nomd) md <- FALSE
  }

  markdown_on(md)
}
