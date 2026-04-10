# Tags for documenting R6

Learn the full details in
[`vignette('rd-R6')`](https://roxygen2.r-lib.org/dev/articles/rd-R6.md).

Key tags:

- `@field ${1:name} ${2:description}`: Describe a R6 or refClass field.

- `@R6method ${1:Class}$${2:method}`: Document an R6 method that can't
  be discovered by introspection, such as methods added via `$set()`.

## Usage

``` r
#' @field ${1:name} ${2:description}
#' @R6method ${1:Class}$${2:method}
```

## See also

Other documentation tags:
[`tags-index-crossref`](https://roxygen2.r-lib.org/dev/reference/tags-index-crossref.md),
[`tags-rd-S3`](https://roxygen2.r-lib.org/dev/reference/tags-rd-S3.md),
[`tags-rd-S4`](https://roxygen2.r-lib.org/dev/reference/tags-rd-S4.md),
[`tags-rd-S7`](https://roxygen2.r-lib.org/dev/reference/tags-rd-S7.md),
[`tags-rd-datasets`](https://roxygen2.r-lib.org/dev/reference/tags-rd-datasets.md),
[`tags-rd-functions`](https://roxygen2.r-lib.org/dev/reference/tags-rd-functions.md),
[`tags-reuse`](https://roxygen2.r-lib.org/dev/reference/tags-reuse.md)
