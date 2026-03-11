# Parse tags

These functions parse the `raw` tag value, convert a string into a
richer R object and storing it in `val`, or provide an informative
warning and returning `NULL`.

## Usage

``` r
tag_value(x)

tag_inherit(x)

tag_name(x)

tag_two_part(x, first, second, required = TRUE, markdown = TRUE)

tag_name_description(x)

tag_words(x, min = 0, max = Inf)

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
