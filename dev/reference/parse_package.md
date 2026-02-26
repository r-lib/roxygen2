# Parse a package, file, or inline code

`parse_package()`, `parse_file()`, and `parse_text()` allow you to use
roxygen's parsing code to parse the roxygen blocks from a package, file,
or character vector of code. `env_package()` and `env_file()` provide
defaults that generate a temporary environment making it possible to
associate each block with the corresponding live object.

## Usage

``` r
parse_package(path = ".", env = env_package(path))

parse_file(file, env = env_file(file), srcref_path = NULL)

parse_text(text, env = env_file(file))

env_file(file)

env_package(path)
```

## Arguments

- path, file, text:

  Either specify a `path` to the root directory of a package, an R
  `file`, or a character vector `text`.

- env:

  An environment environment containing the result of evaluating the
  input code. The defaults will do this for you in a test environment:
  for real code you'll need to generate the environment yourself.

  You can also set to `NULL` if you only want to get the tokenized code
  blocks only. This suppresses evaluation of `@eval` tags, and will not
  find the code object associated with each block.

## Value

A list of roxy_block objects
