# Escape fragile Rd tags

`escape_rd_for_md()` replaces fragile Rd tags with placeholders, to
avoid interpreting them as markdown. `unescape_rd_for_md()` puts the
original text back in place of the placeholders after the markdown
parsing is done. The fragile tags are listed in `escaped_for_md`.

Some Rd macros are treated specially:

- For `if`, markdown is only allowed in the second argument.

- For `ifelse` markdown is allowed in the second and third arguments.

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

- `escape_rd_for_md`: a "safe" version of the input text, where each
  fragile Rd tag is replaced by a placeholder. The original text is
  added as an attribute for each placeholder.

- `unescape_rd_for_md`: the original Rd text.
