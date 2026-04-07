#' @export
roxy_tag_parse.roxy_tag_examples <- function(x) {
  tag_examples(x)
}
#' @export
roxy_tag_parse.roxy_tag_examplesIf <- function(x) {
  lines <- unlist(strsplit(x$raw, "\r?\n"))

  condition <- lines[1]
  parse_err <- tryCatch(
    {
      suppressWarnings(parse(text = condition))
      NULL
    },
    error = function(err) err
  )
  if (!is.null(parse_err)) {
    warn_roxy_tag(x, "condition failed to parse", parent = parse_err)
    return(NULL)
  }

  example_lines <- lines[-1]
  if (length(example_lines) == 0 || all(trimws(example_lines) == "")) {
    warn_roxy_tag(x, "requires example code after the condition")
    return(NULL)
  }

  x$raw <- paste(
    c(
      paste0("\\dontshow{if (", condition, ") withAutoprint(\\{ # examplesIf}"),
      lines[-1],
      "\\dontshow{\\}) # examplesIf}"
    ),
    collapse = "\n"
  )

  tag_examples(x)
}
#' @export
roxy_tag_parse.roxy_tag_example <- function(x) {
  x <- tag_value(x, multiline = TRUE)

  nl <- re_count(x$val, "\n")
  if (any(nl) > 0) {
    warn_roxy_tag(
      x,
      c(
        "must be a single line",
        i = "Do you want @examples?"
      )
    )
    return()
  }

  x
}

#' @export
roxy_tag_rd.roxy_tag_examples <- function(x, base_path, env) {
  rd_section("examples", x$val)
}
#' @export
roxy_tag_rd.roxy_tag_examplesIf <- function(x, base_path, env) {
  rd_section("examples", x$val)
}
#' @export
roxy_tag_rd.roxy_tag_example <- function(x, base_path, env) {
  path <- file.path(base_path, x$val)
  if (!file.exists(path)) {
    warn_roxy_tag(x, "{.path {path}} doesn't exist")
    return()
  }

  code <- read_lines(path)
  rd_section("examples", escape_examples(code))
}

#' @export
format.rd_section_examples <- function(x, ...) {
  value <- paste0(x$value, collapse = "\n")
  rd_macro(x$type, value, space = TRUE)
}

#' Escape examples
#'
#' This documentation topic is used primarily for testing and to record
#' our understanding of the `\example{}` escaping rules.
#' See <https://developer.r-project.org/parseRd.pdf> for the details provided
#' by R core.
#'
#' @keywords internal
#' @export
#' @examples
#' # In examples we automatically escape Rd comments (%):
#' 100 %% 30
#' # even if they are in strings
#' "50%"
#'
#' # and \\ and \v inside of strings and symbols
#' "\v" # vertical tab
#' "\\"
#' # but not comments: \l \v
#'
#' # other string escapes are left as is
#' "\""
#' "\n"
#'
#' # Otherwise, backslashes and parentheses are left as is. This
#' # means that you need to escape unbalanced parentheses, which typically only
#' # occur in \dontshow{}:
#' \dontshow{if (FALSE) \{ }
#' print("Hello")
#' \dontshow{ \} }
#'
#' # You also need to escape backslashes in infix operators and comments
#' # (this is generally rare)
#' `%\\%` <- function(x, y) x + y
#' 10 %\\% 20
#' # \\\\ (renders as two backslashes)
escape_examples <- function(x) {
  x <- paste0(x, collapse = "\n")
  rd(escapeExamples(x))
}

# Strip \dontrun{}, \donttest{}, and \dontshow{} from example code.
# Used for R6 method examples displayed in \preformatted{} blocks where
# Rd macros are not interpreted. \dontrun{} and \donttest{} are unwrapped
# (content kept), while \dontshow{} is removed entirely.
strip_rd_example_tags <- function(x) {
  # Process each tag type
  for (tag in c("\\dontrun", "\\donttest")) {
    x <- strip_rd_tag(x, tag, keep_contents = TRUE)
  }
  x <- strip_rd_tag(x, "\\dontshow", keep_contents = FALSE)
  # Clean up any resulting blank lines at start/end
  x <- gsub("^\n+", "", x)
  x <- gsub("\n+$", "", x)

  x
}

strip_rd_tag <- function(x, tag, keep_contents) {
  needle <- paste0(tag, "{")
  repeat {
    start <- regexpr(needle, x, fixed = TRUE)
    if (start == -1L) {
      break
    }

    tag_len <- attr(start, "match.length")
    # findEndOfTag uses 0-based indexing
    open_brace_0 <- start + tag_len - 2L
    end_0 <- findEndOfTag(x, is_code = TRUE, start = open_brace_0)
    if (end_0 == -1L) {
      break
    }

    close_pos <- end_0 + 1L

    inner <- substring(x, start + tag_len, close_pos - 1L)
    if (keep_contents) {
      # Remove leading/trailing whitespace from inner content
      inner <- gsub("^\n", "", inner)
      inner <- gsub("\n$", "", inner)
    } else {
      inner <- ""
    }

    before <- substring(x, 1L, start - 1L)
    after <- substring(x, close_pos + 1L)

    # Clean up surrounding newlines
    before <- gsub("\n$", "", before)
    if (nzchar(inner)) {
      after <- gsub("^\n", "", after)
      x <- paste0(
        before,
        if (nzchar(before)) "\n",
        inner,
        if (nzchar(after)) "\n",
        after
      )
    } else {
      after <- gsub("^\n", "", after)
      x <- paste0(before, if (nzchar(before) && nzchar(after)) "\n", after)
    }
  }

  x
}
