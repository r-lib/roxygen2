#' @export
roxy_tag_parse.roxy_tag_backref <- function(x) {
  tag_value(x)
}

#' @export
roxy_tag_rd.roxy_tag_backref <- function(x, base_path, env) {
  rd_section("backref", x$val)
}

#' @export
format.rd_section_backref <- function(x, ...) {
  filename <- unique(x$value)
  filename <- file.path(
    basename(dirname(filename)),
    basename(filename),
    fsep = "/"
  )

  lines <- stringi::stri_wrap(
    paste0("Please edit documentation in ", paste(filename, collapse = ", ")),
    initial = "% ",
    prefix = "%   ",
    width = 80,
    whitespace_only = TRUE
  )

  paste0(paste0(lines, collapse = "\n"))
}
