# Tags that help you reuse documentation

Learn the full details in
[`vignette('reuse')`](https://roxygen2.r-lib.org/dev/articles/reuse.md).

Key tags:

- `@describeIn ${1:destination} ${2:description}`: Document a function
  or method in the `destination` topic.

- `@inherit ${1:source} ${2:components}`: Inherit one or more
  documentation components from another topic. If `components` is
  omitted, all supported components will be inherited. Otherwise,
  specify individual components to inherit by picking one or more of
  `params`, `return`, `title`, `description`, `details`, `seealso`,
  `sections`, `references`, `examples`, `author`, `source`, `note`, and
  `format`.

- `@inheritDotParams ${1:source} ${2:arg1 arg2 arg3}`: Automatically
  generate documentation for `...` when you're passing dots along to
  another function.

- `@inheritParams ${1:source}`: Inherit argument documentation from
  another function. Only inherits documentation for arguments that
  aren't already documented locally.

- `@inheritSection ${1:source} ${2:section name}`: Inherit a specific
  named section from another topic.

- `@order ${1:number}`: Override the default (lexigraphic) order in
  which multiple blocks are combined into a single topic.

- `@rdname ${1:topic-name}`: Override the file name of generated `.Rd`
  file. Can be used to combine multiple blocks into a single
  documentation topic.

Other less frequently used tags:

- `@eval ${1:r-code}`: Evaluate arbitrary code in the package namespace
  and insert the results back into the block. Should return a character
  vector of lines.

- `@evalRd ${1:r-code}`: Evaluate arbitrary code in the package
  namespace and insert the results back as into the block. Should return
  a character vector of lines.

- `@includeRmd man/rmd/${1:filename}.Rmd`: Insert the contents of an
  `.Rmd` into the current block. Superseded in favour of using a code
  chunk with a child document.

- `@template ${1:path-to-template}`: Use a roxygen2 template. Now
  superseded in favour of inline R code.

- `@templateVar ${1:name} ${2:value}`: Define variables for use in a
  roxygen2 template.

## Usage

``` r
#' @describeIn ${1:destination} ${2:description}
#' @eval ${1:r-code}
#' @evalRd ${1:r-code}
#' @includeRmd man/rmd/${1:filename}.Rmd
#' @inherit ${1:source} ${2:components}
#' @inheritDotParams ${1:source} ${2:arg1 arg2 arg3}
#' @inheritParams ${1:source}
#' @inheritSection ${1:source} ${2:section name}
#' @order ${1:number}
#' @rdname ${1:topic-name}
#' @template ${1:path-to-template}
#' @templateVar ${1:name} ${2:value}
```

## See also

Other documentation tags:
[`tags-index-crossref`](https://roxygen2.r-lib.org/dev/reference/tags-index-crossref.md),
[`tags-rd`](https://roxygen2.r-lib.org/dev/reference/tags-rd.md),
[`tags-rd-other`](https://roxygen2.r-lib.org/dev/reference/tags-rd-other.md)
