# Update Collate field in DESCRIPTION

By default, R loads files in alphabetical order. This is fine if your
package doesn't have any cross-file dependencies, but if you're using a
tool like S4, you'll need to make sure that classes are loaded before
subclasses and generics are defined before methods. You can do this by
hand by setting the `Collate` field in the `DESCRIPTION` or automate it
by using `@include` tags to specify the cross-file dependencies:

    #' @include before.R
    NULL

If there are no `@include` tags, roxygen2 will leave the `Collate` field
as is. This makes it easier to use roxygen2 with an existing collate
directive, but if you remove all your `@include` tags, you'll need to
also manually delete the collate field.

Generally, you should not need to run this function yourself; it will be
run automatically by any package that needs to load your R files in
collation order.

`update_collate()` is not not technically a
[roclet](https://roxygen2.r-lib.org/dev/reference/roclet.md), like
[`rd_roclet()`](https://roxygen2.r-lib.org/dev/reference/rd_roclet.md)
and
[`namespace_roclet()`](https://roxygen2.r-lib.org/dev/reference/namespace_roclet.md),
because you have to be able to load the pacakge before you can process
it with roclets. However, because it was historical implemented as a
roclet, it's still controlled by the `roclets` argument of
[`roxygenize()`](https://roxygen2.r-lib.org/dev/reference/roxygenize.md).

## Usage

``` r
update_collate(base_path)
```

## Arguments

- base_path:

  Path to package directory.

## Examples

``` r
#' If `example-a.R`, `example-b.R` and `example-c.R` live in `R/`
#' and we're in `example-a.R`, then the following @include tag
#' ensures that example-b and example-c are sourced before example-a.
#' @include example-b.R example-c.R
NULL
#> NULL
```
