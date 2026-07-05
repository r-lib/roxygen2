# Tokenizing the combined markdown/Rd grammar
#
# `md_tokenize()` replaces every backslash-initiated construct with an
# inert placeholder, "\uE000<index>\uE001", so that commonmark can parse
# the remaining text as pure markdown, with no escaping. See
# src/tokenizeMd.cpp for the grammar. `restore_tokens()` substitutes the
# original constructs back while the parsed markdown is translated to Rd;
# the `mode` argument selects how a token is rendered in the Rd context
# it lands in:
#
# * `text`: regular Rd text. Verbatim tags come back as live Rd; the
#   markdown bracket escapes `\[` and `\]` drop their backslash (their
#   only purpose was to hide the bracket from the markdown parser);
#   everything else comes back as typed.
# * `verb`: inside `\verb{}`, `\code{}` or `\preformatted{}`. Everything
#   renders literally, so token text is Rd-escaped -- except verbatim Rd
#   tags, which are inserted as typed: the Rd parser keeps unknown macros
#   in verbatim contexts as literal text, and this is how the escaping
#   code always behaved.
# * `raw`: unprocessed output, e.g. the body of a generated `\Sexpr{}`.

verbatim_rd_tags <- c(
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

md_tokenize <- function(text) {
  out <- tokenizeMd(text, verbatim_rd_tags)
  if (out$stripped > 0) {
    cli::cli_warn(
      "Removed {out$stripped} private-use unicode character{?s} (U+E000/U+E001), which roxygen2 uses internally."
    )
  }
  out
}

restore_tokens <- function(x, state, mode = c("text", "verb", "raw")) {
  if (length(state$tokens) == 0 || !grepl("\uE000", x, fixed = TRUE)) {
    return(x)
  }
  mode <- match.arg(mode)

  m <- gregexpr("\uE000[0-9]+\uE001", x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(placeholder) {
    idx <- as.integer(gsub("[\uE000\uE001]", "", placeholder))
    src <- state$tokens[idx]
    type <- state$types[idx]

    switch(
      mode,
      raw = src,
      text = ifelse(src %in% c("\\[", "\\]"), substr(src, 2, 2), src),
      verb = ifelse(type == "verbatim", src, escape_rd_verb(src))
    )
  })
  x
}

escape_rd_verb <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("{", "\\{", x, fixed = TRUE)
  x <- gsub("}", "\\}", x, fixed = TRUE)
  x
}
