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

roxy_tag_eval <- function(tag, env) {
  tryCatch({
    expr <- parse(text = tag$val)
    out <- eval(expr, envir = env)

    if (!is.character(out)) {
      roxy_tag_warning(tag, "did not evaluate to a string")
    } else if (anyNA(out)) {
      roxy_tag_warning(tag, "result contained NA")
    } else {
      out
    }
  }, error = function(e) {
    roxy_tag_warning(tag, "failed with error:\n", e$message)
  })
}
