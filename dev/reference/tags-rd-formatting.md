# Tags related to markdown support

Learn the full details in
[`vignette('rd-formatting')`](https://roxygen2.r-lib.org/dev/articles/rd-formatting.md).

Other less frequently used tags:

- `@md`: Force markdown processing for a block.

- `@noMd`: Suppress markdown processing for a block.

- `@section ${1:section title}: `: Add an arbitrary section to the
  documentation. Now generally superseded in favour of using a level 1
  heading.

## Usage

``` r
#' @md
#' @noMd
#' @section ${1:section title}:
```
