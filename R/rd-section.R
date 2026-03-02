#' @export
roxy_tag_parse.roxy_tag_section <- function(x) {
  tag_markdown(x)
}

#' @export
roxy_tag_rd.roxy_tag_section <- function(x, base_path, env) {
  pieces <- str_split(x$val, ":", n = 2)[[1]]
  title <- str_split(pieces[1], "\n")[[1]]

  if (length(title) > 1) {
    warn_roxy_tag(
      x,
      c(
        "title spans multiple lines.",
        i = "Did you forget a colon (:) at the end of the title?"
      )
    )
    return()
  }

  rd_section_section(pieces[1], pieces[2])
}

rd_section_section <- function(title, content) {
  stopifnot(is.character(title), is.character(content))
  stopifnot(length(title) == length(content))

  rd_section("section", list(title = title, content = content))
}

#' @export
format.rd_section_section <- function(x, ...) {
  paste0(
    "\\section{",
    x$value$title,
    "}{\n",
    x$value$content,
    "\n}\n",
    collapse = "\n"
  )
}

#' @export
merge.rd_section_section <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))

  dedup <- collapse(
    c(x$value$title, y$value$title),
    c(x$value$content, y$value$content),
    paste,
    collapse = "\n\n"
  )
  rd_section("section", list(title = dedup$key, content = unlist(dedup$value)))
}
