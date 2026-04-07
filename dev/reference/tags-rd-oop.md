# Tags for documenting classes

Learn the full details in
[`vignette('rd-other')`](https://roxygen2.r-lib.org/dev/articles/rd-other.md).

Key tags:

- `@field ${1:name} ${2:description}`: Describe a R6 or refClass field.

- `@method ${1:generic} ${2:class}`: Force a function to be recognised
  as an S3 method. This affects the default usage and the `NAMESPACE`
  directive produced by `@export`. Only needed if automatic detection
  fails.

- `@prop ${1:name} ${2:description}`: Describe an S7 class property that
  is not a constructor parameter.

- `@slot ${1:name} ${2:description}`: Describe the slot of an S4 class.

Other less frequently used tags:

- `@R6method ${1:Class}$${2:method}`: Document an R6 method that can't
  be discovered by introspection, such as methods added via `$set()`.

## Usage

``` r
#' @field ${1:name} ${2:description}
#' @method ${1:generic} ${2:class}
#' @prop ${1:name} ${2:description}
#' @R6method ${1:Class}$${2:method}
#' @slot ${1:name} ${2:description}
```

## See also

Other documentation tags:
[`tags-index-crossref`](https://roxygen2.r-lib.org/dev/reference/tags-index-crossref.md),
[`tags-rd`](https://roxygen2.r-lib.org/dev/reference/tags-rd.md),
[`tags-rd-data`](https://roxygen2.r-lib.org/dev/reference/tags-rd-data.md),
[`tags-reuse`](https://roxygen2.r-lib.org/dev/reference/tags-reuse.md)
