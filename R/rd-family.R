#' @export
roxy_tag_parse.roxy_tag_family <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_family <- function(x, base_path, env) {
  rd_section(x$tag, x$val)
}
#' @export
format.rd_section_family <- function(x, ...) {
  NULL
}

# -------------------------------------------------------------------------

topics_process_family_prefix <- function(family) {
  default <- paste0("Other ", family, ": ")

  # check for meta (use default prefix when unset)
  meta <- roxy_meta_get("rd_family_title")
  if (is.null(meta)) {
    return(default)
  }

  # validate meta structure
  valid <- is.character(meta) || is.list(meta)
  if (!valid) {
    cli::cli_abort(
      "{.code rd_family_title} is set, but is not a named list / vector"
    )
  }

  # extract element
  prefix <- meta[[family]]
  if (is.null(prefix)) {
    return(default)
  }

  markdown(prefix, tag = "family")
}

topics_process_family <- function(topics, env) {
  family_index <- invert(topics$simple_values("family"))
  aliases <- topics$simple_values("alias")

  for (topic_name in names(topics$topics)) {
    topic <- topics$get(topic_name)
    families <- topic$get_value("family")

    for (family in families) {
      related <- family_index[[family]]
      topic$add(rd_section("concept", family))

      others <- setdiff(related, topic_name)
      if (length(others) < 1) {
        next
      }

      other_aliases <- aliases[others]
      other_aliases_order <- map_chr(other_aliases, \(x) escape(x[1]))

      by_file <- map_chr(
        other_aliases[order_c(other_aliases_order)],
        function(x) {
          obj <- find_object(x[1], env)
          suffix <- if (is.function(obj$value)) "()" else ""
          paste0("\\code{\\link{", escape(x[1]), "}", suffix, "}")
        }
      )
      links <- paste(by_file, collapse = ",\n")
      seealso <- topics_process_family_prefix(family)

      topic$add(rd_section("seealso", paste0(seealso, "\n", links)))
    }
  }

  invisible()
}
