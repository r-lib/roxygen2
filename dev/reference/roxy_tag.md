# `roxy_tag` S3 constructor

`roxy_tag()` is the constructor for tag objects. `roxy_tag_warning()` is
superseded by `warn_roxy_tag()`; use to generate a warning that includes
the location of the tag.

## Usage

``` r
roxy_tag(tag, raw, val = NULL, file = NA_character_, line = NA_character_)

roxy_tag_parse(x)

roxy_tag_warning(x, ...)

warn_roxy_tag(tag, message, parent = NULL, envir = parent.frame())
```

## Arguments

- tag:

  Tag name. Arguments starting with `.` are reserved for internal usage.

- raw:

  Raw tag value, a string.

- val:

  Parsed tag value, typically a character vector, but sometimes a list.
  Usually filled in by `tag_parsers`

- file, line:

  Location of the tag

- x:

  A tag

## Methods

Define a method for `roxy_tag_parse` to support new tags. See
[tag_parsers](https://roxygen2.r-lib.org/dev/reference/tag_parsers.md)
for more details.
