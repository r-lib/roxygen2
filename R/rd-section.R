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

  roxy_field("section", list(title = title, content = content))
}

#' @export
format.roxy_field_section <- function(x, ...) {
  paste0(
    "\\section{", x$value$title, "}{\n", x$value$content, "\n}\n",
    collapse = "\n"
  )
}

#' @export
merge.roxy_field_section <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))

  dedup <- collapse(
    c(x$value$title, y$value$title),
    c(x$value$content, y$value$content),
    paste, collapse = "\n\n"
  )
  roxy_field("section", list(title = dedup$key, content = unlist(dedup$value)))
}
