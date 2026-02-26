# Check markdown escaping

This is a regression test for Markdown escaping.

## Usage

``` r
double_escape_md(text)
```

## Arguments

- text:

  Input text.

## Value

Double-escaped text.

## Details

Each of the following bullets should look the same when rendered:

- Backticks: `\`, `\%`, `\$`, `\_`

- `\verb{}`: `\`, `\%`, `\$`, `\_`

\[ this isn't a link \] \\ neither is this \\

## Examples

``` r
"%" # percent
#> [1] "%"
"\"" # double quote
#> [1] "\""
'\'' # single quote
#> [1] "'"
```
