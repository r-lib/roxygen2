#' @export
roxy_tag_parse.roxy_tag_family <- function(x) tag_value(x)
#' @export
roxy_tag_rd.roxy_tag_family <- function(x, base_path, env) {
  roxy_field(x$tag, x$val)
}
#' @export
format.roxy_field_family <- function(x, ...) {
  NULL
}

# -------------------------------------------------------------------------

topics_process_family_prefix <- function(family) {

  default <- paste0("Other ", family, ": ")

  # check for meta (use default prefix when unset)
  meta <- roxy_meta_get("rd_family_title")
  if (is.null(meta))
    return(default)

  # validate meta structure
  valid <- is.character(meta) || is.list(meta)
  if (!valid) {
    message <- "rd_family_title is set, but is not a named list / vector"
    abort(message)
  }

  # extract element
  prefix <- meta[[family]]
  if (is.null(prefix))
    return(default)

  prefix

}

topics_process_family <- function(topics, env) {
  family_index <- invert(topics$simple_values("family"))
  aliases <- topics$simple_values("alias")

  for (topic_name in names(topics$topics)) {
    topic <- topics$get(topic_name)
    families <- topic$get_value("family")

    for (family in families) {
      related <- family_index[[family]]
      topic$add_simple_field("concept", family)

      others <- setdiff(related, topic_name)
      if (length(others) < 1)
        next

      by_file <- map_chr(aliases[others], function(x) {
        obj <- find_object(x[1], env)
        suffix <- if (is.function(obj$value)) "()" else ""
        paste0("\\code{\\link{", escape(x[1]), "}", suffix, "}")
      })
      links <- paste(sort_c(by_file), collapse = ", ")

      seealso <- topics_process_family_prefix(family)
      out <- strwrap(links, initial = seealso, width = 60, exdent = 2)

      topic$add_simple_field("seealso", paste(out, collapse = "\n"))
    }
  }

  invisible()
}
