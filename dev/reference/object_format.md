# Default format for data

This function is called to generate the default "Format" section for
each data object. The default implementation will return the class and
dimension information.

## Usage

``` r
object_format(x)
```

## Arguments

- x:

  A data object

## Value

A `character` value with valid `Rd` syntax, or `NULL`.
