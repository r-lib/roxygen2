# Determine if a function is an S3 generic or S3 method

`is_s3_generic` compares name to `.knownS3Generics` and
`.S3PrimitiveGenerics`, then looks at the function body to see if it
calls [`UseMethod()`](https://rdrr.io/r/base/UseMethod.html).

`is_s3_method` builds names of all possible generics for that function
and then checks if any of them actually is a generic.

## Usage

``` r
is_s3_generic(name, env = parent.frame())

is_s3_method(name, env = parent.frame())
```

## Arguments

- name:

  Name of function.

- env:

  Base environment in which to look for function definition.
