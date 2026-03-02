#' @export
#' @rdname roxy_tag
warn_roxy_tag <- function(tag, message, parent = NULL, envir = parent.frame()) {
  tag_name <- cli::format_inline("{.strong @{tag$tag}} ")
  if (is.null(tag$raw)) {
    tag_name <- paste(tag_name, "(automatically generated) ")
  }
  message[[1]] <- paste0(tag_name, message[[1]])

  warn_roxy(tag$file, tag$line, message, parent = parent, envir = envir)
}

warn_roxy_block <- function(
  block,
  message,
  parent = NULL,
  envir = parent.frame()
) {
  warn_roxy(block$file, block$line, message, parent = parent, envir = envir)
}

warn_roxy_function <- function(
  fun,
  message,
  parent = NULL,
  envir = parent.frame()
) {
  srcref <- attr(fun, "srcref")
  file <- attr(srcref, "srcfile")$filename
  line <- as.vector(srcref)[[1]]

  warn_roxy(file, line, message, parent = parent, envir = envir)
}

warn_roxy <- function(
  file,
  line,
  message,
  parent = NULL,
  envir = parent.frame()
) {
  link <- cli::style_hyperlink(
    paste0(basename(file), ":", line),
    paste0("file://", file),
    params = c(line = line, col = 1)
  )

  message[[1]] <- paste0(link, ": ", message[[1]], ".")
  names(message)[[1]] <- "x"
  cli::cli_inform(message, parent = parent, .envir = envir)
}

warn_roxy_topic <- function(
  topic,
  message,
  parent = NULL,
  envir = parent.frame()
) {
  message[[1]] <- paste0("In topic '", topic, "': ", message[[1]], ".")
  names(message)[[1]] <- "x"
  cli::cli_inform(message, parent = parent, .envir = envir)
}
