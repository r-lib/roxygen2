# Build a new roclet

Roclets are roxygen2's plugin system for producing different types of
output, like Rd files
([`rd_roclet()`](https://roxygen2.r-lib.org/dev/reference/rd_roclet.md))
or the `NAMESPACE` file
([`namespace_roclet()`](https://roxygen2.r-lib.org/dev/reference/namespace_roclet.md)).

To create a new roclet, you will need to create a constructor function
that calls `roclet()`, and then implement the methods described below.

See
[`vignette("extending")`](https://roxygen2.r-lib.org/dev/articles/extending.md)
for more details.

## Usage

``` r
roclet(subclass, ...)

roclet_preprocess(x, blocks, base_path)

roclet_process(x, blocks, env, base_path)

roclet_output(x, results, base_path, ...)

roclet_clean(x, base_path)

roclet_tags(x)
```

## Arguments

- subclass:

  Class of the roclet, character vector.

- x:

  A `roclet` object.

- blocks:

  A list of
  [roxy_block](https://roxygen2.r-lib.org/dev/reference/roxy_block.md)
  objects.

- base_path:

  Path to root of source package.

- env:

  Package environment.

- results:

  Value returned from your `roclet_process()` method.

## Methods

- `roclet_preprocess()` is called after blocks have been parsed but
  before code has been evaluated. This should only be needed if your
  roclet affects how code will be evaluated. Should return a roclet.

- `roclet_process()` called after blocks have been evaluated; i.e. the
  `@eval` tag has been processed, and the object associated with each
  block has been determined.

- `roclet_output()` is given the output from `roclet_process()` and
  should produce files on disk.

- `roclet_clean()` called when `roxygenise(clean = TRUE)`. Should remove
  any files created by the roclet.

### Deprecated methods

`roclet_tags()` is no longer used; instead provide a
[`roxy_tag_parse()`](https://roxygen2.r-lib.org/dev/reference/roxy_tag.md)
method for each tag.

## Examples

``` r
# Custom roclet
custom_roclet <- roclet("custom")
# Roclet that extends the existing Rd roclet.
supercharged_rd_roclet <- roclet(c("cool", "rd"))
```
