# Tokenizing the combined markdown/Rd grammar
#
# `md_tokenize()` replaces every backslash-initiated construct with an
# inert placeholder, "\uE000<index>\uE001", so that commonmark can parse
# the remaining text as pure markdown, with no escaping. See
# src/tokenizeMd.cpp for the grammar. The C++ tree walk
# (src/mdxmlToRd.cpp) substitutes the original constructs back while the
# parsed markdown is translated to Rd, rendering each token according to
# the Rd context it lands in.

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

md_tokenize <- function(text, tag = NULL) {
  out <- tokenizeMd(text, verbatim_rd_tags)
  if (out$stripped > 0) {
    cli::cli_warn(
      "Removed {out$stripped} private-use unicode character{?s} (U+E000/U+E001), which roxygen2 uses internally."
    )
  }
  for (name in unique(out$incomplete)) {
    warn_roxy_tag(
      tag,
      c(
        "markdown translation failed",
        x = paste0("\\", name, " has an unterminated argument"),
        i = "Rd tag arguments must have balanced braces, and an unescaped % comments out the rest of the line: write \\% for a literal %"
      )
    )
  }
  out
}

