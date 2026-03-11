# Create a roclet from a string

This provides a flexible way of specifying a roclet in a string.

## Usage

``` r
roclet_find(x)
```

## Arguments

- x:

  Arbitrary R code evaluated in roxygen2 package.

## See also

Other extending:
[`load_options()`](https://roxygen2.r-lib.org/dev/reference/load_options.md),
[`parse_package()`](https://roxygen2.r-lib.org/dev/reference/parse_package.md),
[`rd_section()`](https://roxygen2.r-lib.org/dev/reference/rd_section.md),
[`roc_proc_text()`](https://roxygen2.r-lib.org/dev/reference/roc_proc_text.md),
[`roxy_block()`](https://roxygen2.r-lib.org/dev/reference/roxy_block.md),
[`roxy_tag()`](https://roxygen2.r-lib.org/dev/reference/roxy_tag.md),
[`roxy_tag_rd()`](https://roxygen2.r-lib.org/dev/reference/roxy_tag_rd.md),
[`tag_parsers`](https://roxygen2.r-lib.org/dev/reference/tag_parsers.md),
[`tags_list()`](https://roxygen2.r-lib.org/dev/reference/tags_list.md)

## Examples

``` r
# rd, namespace, and vignette work for backward compatibility
roclet_find("rd")
#> list()
#> attr(,"class")
#> [1] "roclet_rd" "roclet"   

# But generally you should specify the name of a function that
# returns a roclet
roclet_find("rd_roclet")
#> list()
#> attr(,"class")
#> [1] "roclet_rd" "roclet"   

# If it lives in another package, you'll need to use ::
roclet_find("roxygen2::rd_roclet")
#> list()
#> attr(,"class")
#> [1] "roclet_rd" "roclet"   

# If it takes parameters (which no roclet does currently), you'll need
# to call the function
roclet_find("roxygen2::rd_roclet()")
#> list()
#> attr(,"class")
#> [1] "roclet_rd" "roclet"   
```
