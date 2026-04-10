# Tags for documenting S3

Learn the full details in
[`vignette('rd-S3')`](https://roxygen2.r-lib.org/dev/articles/rd-S3.md).

Key tags:

- `@method ${1:generic} ${2:class}`: Force a function to be recognised
  as an S3 method. This affects the default usage and the `NAMESPACE`
  directive produced by `@export`. Only needed if automatic detection
  fails.

## Usage

``` r
#' @method ${1:generic} ${2:class}
```

## See also

Other documentation tags:
[`tags-index-crossref`](https://roxygen2.r-lib.org/dev/reference/tags-index-crossref.md),
[`tags-rd-R6`](https://roxygen2.r-lib.org/dev/reference/tags-rd-R6.md),
[`tags-rd-S4`](https://roxygen2.r-lib.org/dev/reference/tags-rd-S4.md),
[`tags-rd-S7`](https://roxygen2.r-lib.org/dev/reference/tags-rd-S7.md),
[`tags-rd-datasets`](https://roxygen2.r-lib.org/dev/reference/tags-rd-datasets.md),
[`tags-rd-functions`](https://roxygen2.r-lib.org/dev/reference/tags-rd-functions.md),
[`tags-reuse`](https://roxygen2.r-lib.org/dev/reference/tags-reuse.md)
