# Blocks

A `roxy_block` represents a single roxygen2 block.

The `block_*` functions provide a few helpers for common operations:

- `block_has_tags(blocks, tags)`: does `block` contain any of these
  `tags`?

- `block_get_tags(block, tags)`: get all instances of `tags`

- `block_get_tag(block, tag)`: get single tag. Returns `NULL` if 0,
  throws warning if more than 1.

- `block_get_tag_value(block, tag)`: gets `val` field from single tag.

## Usage

``` r
roxy_block(tags, file, line, call, object = NULL)

block_has_tags(block, tags)

block_get_tags(block, tags)

block_get_tag(block, tag)

block_get_tag_value(block, tag)
```

## Arguments

- tags:

  A list of
  [roxy_tag](https://roxygen2.r-lib.org/dev/reference/roxy_tag.md)s.

- file, line:

  Location of the `call` (i.e. the line after the last line of the
  block).

- call:

  Expression associated with block.

- object:

  Optionally, the object associated with the block, found by
  inspecting/evaluating `call`.

- block:

  A `roxy_block` to manipulate.

- tag:

  A single tag name.

## See also

Other extending:
[`load_options()`](https://roxygen2.r-lib.org/dev/reference/load_options.md),
[`parse_package()`](https://roxygen2.r-lib.org/dev/reference/parse_package.md),
[`rd_section()`](https://roxygen2.r-lib.org/dev/reference/rd_section.md),
[`roc_proc_text()`](https://roxygen2.r-lib.org/dev/reference/roc_proc_text.md),
[`roclet_find()`](https://roxygen2.r-lib.org/dev/reference/roclet_find.md),
[`roxy_tag()`](https://roxygen2.r-lib.org/dev/reference/roxy_tag.md),
[`roxy_tag_rd()`](https://roxygen2.r-lib.org/dev/reference/roxy_tag_rd.md),
[`tag_parsers`](https://roxygen2.r-lib.org/dev/reference/tag_parsers.md),
[`tags_list()`](https://roxygen2.r-lib.org/dev/reference/tags_list.md)

## Examples

``` r
# The easiest way to see the structure of a roxy_block is to create one
# using parse_text:
text <- "
  #' This is a title
  #'
  #' @param x,y A number
  #' @export
  f <- function(x, y) x + y
"

# parse_text() returns a list of blocks, so I extract the first
block <- parse_text(text)[[1]]
block
#> <roxy_block> [<text>:6]
#>   $tag
#>     [line:  2] @title 'This is a title' {parsed}
#>     [line:  4] @param 'x,y A number' {parsed}
#>     [line:  5] @export '' {parsed}
#>     [line:  6] @usage '<generated>' {parsed}
#>     [line:  6] @.formals '<generated>' {parsed}
#>     [line:  6] @backref '<generated>' {parsed}
#>   $call   f <- function(x, y) x + y
#>   $object <function> 
#>     $topic f
#>     $alias f
```
