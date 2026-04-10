#' @export
roxy_tag_parse.roxy_tag_prop <- function(x) {
  x <- tag_two_part(x, "a property name", "a description")
  if (is.null(x)) {
    return()
  }

  # Optionally specify a class
  if (grepl("@", x$val$name, fixed = TRUE)) {
    pieces <- strsplit(x$val$name, "@", fixed = TRUE)[[1]]
    if (length(pieces) != 2 || pieces[[1]] == "" || pieces[[2]] == "") {
      warn_roxy_tag(x, "must have form class@prop")
      return()
    }
    x$val$class <- pieces[[1]]
    x$val$name <- pieces[[2]]
  }

  x
}

#' @export
roxy_tag_rd.roxy_tag_prop <- function(x, base_path, env) {
  rd_section(
    x$tag,
    data.frame(
      class = x$val$class %||% NA_character_,
      name = x$val$name,
      description = x$val$description
    )
  )
}

#' @export
merge.rd_section_prop <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  rd_section(x$type, rbind(x$value, y$value))
}

#' @export
format.rd_section_prop <- function(x, ...) {
  props <- x$value
  classes <- unique(props$class)

  if (identical(classes, NA_character_)) {
    return(rd_section_description(
      "Additional properties",
      paste0("@", props$name),
      props$description
    ))
  }

  sections <- map_chr(classes, function(cls) {
    rows <- props[props$class == cls, ]
    rd_subsection_description(cls, paste0("@", rows$name), rows$description)
  })

  paste0(
    "\\section{Additional properties}{\n\n",
    paste0(sections, collapse = "\n"),
    "}\n"
  )
}
