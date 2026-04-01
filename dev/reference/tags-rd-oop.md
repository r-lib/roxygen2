# Tags for documenting classes

Learn the full details in
[`vignette('rd-other')`](https://roxygen2.r-lib.org/dev/articles/rd-other.md).

Key tags:

- `@field ${1:name} ${2:description}`: Describe a R6 or refClass field.

- `@method ${1:generic} ${2:class}`: Force a function to be recognised
  as an S3 method. This affects the default usage and the `NAMESPACE`
  directive produced by `@export`. Only needed if automatic detection
  fails.

- `@slot ${1:name} ${2:description}`: Describe the slot of an S4 class.

## Usage

``` r
#' @field ${1:name} ${2:description}
#' @method ${1:generic} ${2:class}
#' @slot ${1:name} ${2:description}
```

## See also

Other documentation tags:
[`tags-index-crossref`](https://roxygen2.r-lib.org/dev/reference/tags-index-crossref.md),
[`tags-rd`](https://roxygen2.r-lib.org/dev/reference/tags-rd.md),
[`tags-rd-data`](https://roxygen2.r-lib.org/dev/reference/tags-rd-data.md),
[`tags-reuse`](https://roxygen2.r-lib.org/dev/reference/tags-reuse.md)
