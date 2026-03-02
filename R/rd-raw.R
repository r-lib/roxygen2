#' @export
roxy_tag_parse.roxy_tag_evalRd <- function(x) tag_code(x)
#' @export
roxy_tag_rd.roxy_tag_evalRd <- function(x, base_path, env) {
  rd_section("rawRd", roxy_tag_eval(x, env))
}

#' @export
roxy_tag_parse.roxy_tag_rawRd <- function(x) tag_value(x)
#' @export
roxy_tag_rd.roxy_tag_rawRd <- function(x, base_path, env) {
  rd_section(x$tag, x$val)
}
#' @export
format.rd_section_rawRd <- function(x, ...) {
  paste(x$value, collapse = "\n")
}

roxy_tag_eval <- function(tag, env = new.env(parent = baseenv())) {
  tryCatch(
    {
      out <- roxy_eval(tag$val, env)

      if (!is.character(out)) {
        warn_roxy_tag(tag, "must evaluate to a character vector")
        NULL
      } else if (anyNA(out)) {
        warn_roxy_tag(tag, "must not contain any missing values")
        NULL
      } else {
        out
      }
    },
    error = function(e) {
      warn_roxy_tag(tag, "failed to evaluate", parent = e)
      NULL
    }
  )
}
