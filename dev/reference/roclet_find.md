# Create a roclet from a string

This provides a flexible way of specifying a roclet in a string.

## Usage

``` r
roclet_find(x)
```

## Arguments

- x:

  Arbitrary R code evaluated in roxygen2 package.

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
