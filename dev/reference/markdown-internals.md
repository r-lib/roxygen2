# Escape Rd markup, to avoid interpreting it as markdown

This is needed, if we want to stay compatible with existing markup, even
if markdown mode is switched on. Fragile Rd tags (tags that may contain
markup that can be picked up by the markdown parser), are replaced by
placeholders. After the markdown to Rd conversion is done, the original
text is put back in place of the placeholders.

It puts back the protected fragile Rd commands into the text after the
markdown parsing.

## Usage

``` r
escape_rd_for_md(text)

unescape_rd_for_md(rd_text, esc_text)
```

## Arguments

- text:

  Input text. Potentially contains Rd and/or markdown markup.

- rd_text:

  The markdown parsed and interpreted text.

- esc_text:

  The original escaped text from `escape_rd_for_md()`.

## Value

For `escape_rd_for_md`: A “safe” version of the input text, where each
fragile Rd tag is replaced by a placeholder. The original text is added
as an attribute for each placeholder.

For `unescape_rd_for_md`: Rd text.

## Details

The list of protected Rd tags is in `escaped_for_md`.

Some Rd macros are treated specially:

- For `if`, markdown is only allowed in the second argument.

- For `ifelse` markdown is allowed in the second and third arguments.

See also `roclet-rd.R` for the list of tags that uses the
markdown-enabled parser. Some tags, e.g. `@aliases`, `@backref`, etc.
only use the standard Roxygen parser.
