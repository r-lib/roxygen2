# Parse tags

These functions parse the `raw` tag value, convert a string into a
richer R object and storing it in `val`, or provide an informative
warning and returning `NULL`.

## Usage

``` r
tag_value(x, multiline = "never")

tag_inherit(x)

tag_name(x)

tag_two_part(
  x,
  first,
  second,
  required = TRUE,
  markdown = TRUE,
  multiline = "never"
)

tag_name_description(x)

tag_words(x, min = 0, max = Inf, multiline = "never")

tag_words_line(x)

tag_toggle(x)

tag_code(x)

tag_examples(x)

tag_markdown(x)

tag_markdown_with_sections(x)
```

## Arguments

- x:

  A [roxy_tag](https://roxygen2.r-lib.org/dev/reference/roxy_tag.md)
  object to parse

- multiline:

  Controls how the tag may span multiple lines:

  - `"never"` (the default): the tag must be a single line, and spanning
    multiple lines generates a warning.

  - `"indent"`: the tag may span multiple lines, but continuation lines
    must use a hanging indent (i.e. be indented more than the first
    line). The first line that is not indented (including a blank line)
    ends the tag, and anything after it is ignored, with a warning. Use
    this for tags where multiline input is convenient but a flush line
    almost always signals a missing tag (e.g., `@importFrom`).

  - `"always"`: the tag may span any number of lines and paragraphs. Use
    this for tags where multiline content is expected (e.g., `@usage`,
    `@rawRd`).

  For backward compatibility, `FALSE` and `TRUE` are accepted as
  synonyms for `"never"` and `"always"` respectively.

- first, second:

  Name of first and second parts of two part tags

- required:

  Is the second part required (TRUE) or can it be blank (FALSE)?

- markdown:

  Should the second part be parsed as markdown?

- min, max:

  Minimum and maximum number of words

## Value

A [roxy_tag](https://roxygen2.r-lib.org/dev/reference/roxy_tag.md)
object with the `val` field set to the parsed value.

## New tag

To create a new `@mytag` define `roxy_tag_parse.roxy_tag_mytag()`. It
should either call one of the functions here, or directly set `x$val`.

## See also

Other extending:
[`load_options()`](https://roxygen2.r-lib.org/dev/reference/load_options.md),
[`parse_package()`](https://roxygen2.r-lib.org/dev/reference/parse_package.md),
[`rd_section()`](https://roxygen2.r-lib.org/dev/reference/rd_section.md),
[`roc_proc_text()`](https://roxygen2.r-lib.org/dev/reference/roc_proc_text.md),
[`roclet_find()`](https://roxygen2.r-lib.org/dev/reference/roclet_find.md),
[`roxy_block()`](https://roxygen2.r-lib.org/dev/reference/roxy_block.md),
[`roxy_tag()`](https://roxygen2.r-lib.org/dev/reference/roxy_tag.md),
[`roxy_tag_rd()`](https://roxygen2.r-lib.org/dev/reference/roxy_tag_rd.md),
[`tags_list()`](https://roxygen2.r-lib.org/dev/reference/tags_list.md)
