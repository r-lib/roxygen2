#' Escape fragile Rd tags
#'
#' @description
#' `escape_rd_for_md()` replaces fragile Rd tags with placeholders, to avoid
#' interpreting them as markdown. `unescape_rd_for_md()` puts the original
#' text back in place of the placeholders after the markdown parsing is done.
#'
#' Some Rd macros are treated specially:
#'
#' * For `if`, markdown is only allowed in the second argument.
#' * For `ifelse` markdown is allowed in the second and third arguments.
#'
#' @param text Input text. Potentially contains Rd and/or
#'   markdown markup.
#' @returns
#' * `escape_rd_for_md`: a "safe" version of the input text, where
#'   each fragile Rd tag is replaced by a placeholder. The
#'   original text is added as an attribute for each placeholder.
#' * `unescape_rd_for_md`: the original Rd text.
#' @rdname markdown-internals
#' @keywords internal
escape_rd_for_md <- function(text) {
  result <- escape_rd_for_md_c(text)
  out <- result$text
  attr(out, "roxygen-markdown-subst") <- as.character(result$tags)
  out
}

#' @param rd_text The markdown parsed and interpreted text.
#' @param esc_text The original escaped text from
#'   `escape_rd_for_md()`.
#' @rdname markdown-internals
unescape_rd_for_md <- function(rd_text, esc_text) {
  unescape_rd_for_md_c(rd_text, attr(esc_text, "roxygen-markdown-subst"))
}

#' Check markdown escaping
#'
#' @description
#' This is a regression test for Markdown escaping.
#'
#' Each of the following bullets should look the same when rendered:
#'
#' * Backticks: `\`, `\%`, `\$`, `\_`
#' * `\verb{}`: \verb{\\}, \verb{\\%}, \verb{\$}, \verb{\_}
#'
#' \[ this isn't a link \]
#'
#' \\[ neither is this \\]
#'
#' @name double_escape_md
#' @keywords internal
#' @examples
#' "%" # percent
#' "\"" # double quote
#' '\'' # single quote
NULL
