#' Escape Rd markup, to avoid interpreting it as markdown
#'
#' This is needed, if we want to stay compatible with
#' existing markup, even if markdown mode is switched on.
#' Fragile Rd tags (tags that may contain markup that
#' can be picked up by the markdown parser), are replaced
#' by placeholders. After the markdown to Rd conversion
#' is done, the original text is put back in place of the
#' placeholders.
#'
#' The list of protected Rd tags is in `escaped_for_md`.
#'
#' Some Rd macros are treated specially:
#'
#' * For `if`, markdown is only allowed in the second argument.
#' * For `ifelse` markdown is allowed in the second and third arguments.
#'
#' See also `roclet-rd.R` for the list of tags that
#' uses the markdown-enabled parser. Some tags, e.g.
#' `@aliases`, `@backref`, etc. only use the
#' standard Roxygen parser.
#'
#' @param text Input text. Potentially contains Rd and/or
#'   markdown markup.
#' @return For `escape_rd_for_md`:
#'   A \dQuote{safe} version of the input text, where
#'   each fragile Rd tag is replaced by a placeholder. The
#'   original text is added as an attribute for each placeholder.
#' @rdname markdown-internals
#' @keywords internal

escape_rd_for_md <- function(text) {
  rd_tags <- find_fragile_rd_tags(text, escaped_for_md)
  protected <- protect_rd_tags(text, rd_tags)
  double_escape_md(protected)
}

escaped_for_md <- paste0(
  "\\",
  c(
    "acronym",
    "code",
    "command",
    "CRANpkg",
    "deqn",
    "doi",
    "dontrun",
    "dontshow",
    "donttest",
    "email",
    "env",
    "eqn",
    "figure",
    "file",
    "if",
    "ifelse",
    "kbd",
    "link",
    "linkS4class",
    "method",
    "mjeqn",
    "mjdeqn",
    "mjseqn",
    "mjsdeqn",
    "mjteqn",
    "mjtdeqn",
    "newcommand",
    "option",
    "out",
    "packageAuthor",
    "packageDescription",
    "packageDESCRIPTION",
    "packageIndices",
    "packageMaintainer",
    "packageTitle",
    "pkg",
    "PR",
    "preformatted",
    "renewcommand",
    "S3method",
    "S4method",
    "samp",
    "special",
    "testonly",
    "url",
    "var",
    "verb"
  )
)

#' @description
#' It puts back the protected fragile Rd commands into
#' the text after the markdown parsing.
#'
#' @param rd_text The markdown parsed and interpreted text.
#' @param esc_text The original escaped text from
#'   `escape_rd_for_md()`.
#' @return For `unescape_rd_for_md`: Rd text.
#' @rdname markdown-internals
unescape_rd_for_md <- function(rd_text, esc_text) {
  id <- attr(esc_text, "roxygen-markdown-subst")$id
  tags <- attr(esc_text, "roxygen-markdown-subst")$tags

  for (i in seq_len(nrow(tags))) {
    ph <- paste0(id, "-", i, "-")
    rd_text <- sub(ph, tags$text[i], rd_text, fixed = TRUE)
  }

  rd_text
}

#' Find all fragile tags (int the supplied list) in the text
#'
#' Ignore the tags that are embedded into a fragile tag.
#'
#' @param text Input text, character scalar.
#' @param fragile Character vector of fragile tags to find.
#' @return Data frame of fragile tags, with columns:
#'   `tag`, `start`, `end`, `argend`,
#'   `text`.
#'
#' @noRd

find_fragile_rd_tags <- function(text, fragile) {
  tags <- find_all_rd_tags(text)
  ftags <- tags[tags$tag %in% fragile, ]

  ## Remove embedded ones
  keep <- map_lgl(seq_len(nrow(ftags)), function(i) {
    sum(ftags$start <= ftags$start[i] & ftags$argend >= ftags$argend[i]) == 1
  })

  ftags <- ftags[keep, ]

  if (nrow(ftags)) {
    ftags$text <- str_sub(text, ftags$start, ftags$argend)
  }

  ftags
}

#' Find all (complete) Rd tags in a string
#'
#' Complete means that we include the argument(s) as well.
#'
#' @param text Input text, character scalar.
#'
#' @noRd

find_all_rd_tags <- function(text) {
  text_len <- nchar(text)

  ## Find the tag names
  tags <- find_all_tag_names(text)

  ## Find the end of the argument list for each tag. Note that
  ## tags might be embedded into the arguments of other tags.
  tags$argend <- map_int(seq_len(nrow(tags)), function(i) {
    tag_plus <- str_sub(text, tags$end[i], text_len)
    findEndOfTag(tag_plus, is_code = FALSE) + tags$end[i]
  })

  tags
}

#' Find all tag names in a string
#'
#' Note that we also protect these tags within code, strings
#' and comments, for now. We'll see if this causes any
#' problems.
#'
#' @param text Input text, scalar.
#' @return Data frame, with columns: `tag`, `start`,
#'   `end`.
#'
#' @noRd

find_all_tag_names <- function(text) {
  ## Find the tags without arguments first
  tag_pos <- str_locate_all(text, r"(\\[a-zA-Z][a-zA-Z0-9]*)")[[1]]

  data.frame(
    tag = str_sub(text, tag_pos[, "start"], tag_pos[, "end"]),
    as.data.frame(tag_pos)
  )
}

#' Replace fragile Rd tags with placeholders
#'
#' @param text The text, character scalar.
#' @param rd_tags Fragile Rd tags, in a data frame,
#'   as returned by `find_fragile_rd_tags`.
#' @return Text, after the substitution. The original
#'   text is added as an attribute.
#'
#' @noRd

protect_rd_tags <- function(text, rd_tags) {
  id <- make_random_string()

  text <- str_sub_same(text, rd_tags, id)

  attr(text, "roxygen-markdown-subst") <-
    list(tags = rd_tags, id = id)

  text
}

#' Replace parts of the same string
#'
#' It assumes that the intervals to be replaced do not
#' overlap. Gives an error otherwise.
#'
#' @param str String scalar.
#' @param repl Data frame with columns: `start`, `end`,
#'   `argend`, `text`.
#' @param id Placeholder string.
#' @return Input string with the replacements performed.
#'   Note that all replacements are performed in parallel,
#'   at least conceptually.
#'
#' @noRd

str_sub_same <- function(str, repl, id) {
  repl <- repl[order(repl$start), ]

  if (is.unsorted(repl$end) || is.unsorted(repl$argend)) {
    cli::cli_abort("Replacement intervals must not overlap", .internal = TRUE)
  }

  for (i in seq_len(nrow(repl))) {
    ## The trailing - is needed, to distinguish between -1 and -10
    new_text <- paste0(id, "-", i, "-")
    str_sub(str, repl$start[i], repl$argend[i]) <- new_text

    ## Need to shift other coordinates (we shift everything,
    ## it is just simpler).
    inc <- nchar(new_text) - (repl$argend[i] - repl$start[i] + 1)
    repl$start <- repl$start + inc
    repl$end <- repl$end + inc
    repl$argend <- repl$argend + inc
  }

  str
}

#' Make a random string
#'
#' We use this as the placeholder, to make sure that the
#' placeholder does not appear in the text.
#'
#' @return String scalar
#'
#' @noRd

make_random_string <- function(length = 32) {
  paste(
    sample(c(LETTERS, letters, 0:9), length, replace = TRUE),
    collapse = ""
  )
}

#' Check markdown escaping
#'
#' This is a regression test for Markdown escaping.
#'
#' @details
#' Each of the following bullets should look the same when rendered:
#'
#' * Backticks: `\`, `\%`, `\$`, `\_`
#' * `\verb{}`: \verb{\\}, \verb{\\%}, \verb{\$}, \verb{\_}
#'
#' \[ this isn't a link \]
#' \\[ neither is this \\]
#'
#' @param text Input text.
#' @return Double-escaped text.
#' @keywords internal
#' @examples
#' "%" # percent
#' "\"" # double quote
#' '\'' # single quote
double_escape_md <- function(text) {
  text <- gsub(r"(\)", r"(\\)", text, fixed = TRUE)

  # De-dup escaping used to avoid [] creating a link
  text <- gsub(r"(\\[)", r"(\[)", text, fixed = TRUE)
  text <- gsub(r"(\\])", r"(\])", text, fixed = TRUE)
  text
}
