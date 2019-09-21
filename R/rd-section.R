#' @export
roxy_tag_parse.roxy_tag_section <- function(x) {
  tag_markdown(x)
}

#' @export
roxy_tag_rd.roxy_tag_section <- function(x, base_path, env) {
  pieces <- str_split(x$val, ":", n = 2)[[1]]
  title <- str_split(pieces[1], "\n")[[1]]

  if (length(title) > 1) {
    roxy_tag_warning(x, "Section title spans multiple lines")
    return()
  }

  roxy_field_section(pieces[1], pieces[2])
}

roxy_field_section <- function(title, content) {
  stopifnot(is.character(title), is.character(content))
  stopifnot(length(title) == length(content))

  roxy_field("section", title = title, content = content)
}

#' @export
format.roxy_field_section <- function(x, ..., wrap = TRUE) {
  if (wrap) {
    content <- str_wrap(str_trim(x$content), width = 60, exdent = 2, indent = 2)
  } else {
    content <- x$content
  }

  paste0("\\section{", x$title, "}{\n", content, "\n}\n", collapse = "\n")
}

#' @export
merge.roxy_field_section <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))

  dedup <- collapse(
    c(x$title, y$title),
    c(x$content, y$content),
    paste, collapse = "\n\n"
  )
  roxy_field_section(dedup$key, unlist(dedup$value))
}
