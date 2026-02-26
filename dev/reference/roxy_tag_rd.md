# Generate Rd output from a tag

Provide a method for this generic if you want a tag to generate output
in `.Rd` files. See
[`vignette("extending")`](https://roxygen2.r-lib.org/dev/articles/extending.md)
for more details.

## Usage

``` r
roxy_tag_rd(x, base_path, env)
```

## Arguments

- x:

  The tag

- base_path:

  Path to package root directory.

- env:

  Environment in which to evaluate code (if needed)

## Value

Methods must return a
[rd_section](https://roxygen2.r-lib.org/dev/reference/rd_section.md).
