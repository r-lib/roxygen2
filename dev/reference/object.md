# Constructors for S3 object to represent R objects

These objects are usually created by the parsers, but it is also useful
to generate them by hand for testing.

## Usage

``` r
object(value, alias, type)
```

## Arguments

- value:

  The object itself.

- alias:

  Alias for object being documented, in case you create a generator
  function with different name.
