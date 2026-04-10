# Extending roxygen2

roxygen2 is extensible, and this vignette will show you how. It starts
with an introduction to the basic workflow of
[`roxygenize()`](https://roxygen2.r-lib.org/dev/reference/roxygenize.md)
and the key data structures that power it. Then we’ll show you how you
can use its two extension points:

- Add a new tag to generate a new top-level section in an `.Rd` file.
  This allows you to repeat yourself less when documenting your package.
  (See
  [`vignette("reuse")`](https://roxygen2.r-lib.org/dev/articles/reuse.md)
  for other techniques.)

- Add a new roclet. This lets you take full advantage of the
  computational machinery behind
  [`roxygenize()`](https://roxygen2.r-lib.org/dev/reference/roxygenize.md)
  to compute anything you want or produce any artefact you can imagine.

``` r
library(roxygen2)
```

## How `roxygenize()` works

You’ve probably used
[`roxygenize()`](https://roxygen2.r-lib.org/dev/reference/roxygenize.md)
(or `devtools::document()`) a bunch without ever really thinking about
what’s going on behind the scenes. But if you’re going to extend
roxygen2, you’ll need to know exactly what’s happening:

- Loads the package under roxygenizing, as well as any further packages
  (“packages” in
  [`load_options()`](https://roxygen2.r-lib.org/dev/reference/load_options.md)).

- Parses all R files in the package, using available tags.

- Finds the roclets (see
  [`roclet()`](https://roxygen2.r-lib.org/dev/reference/roclet.md)) from
  its `roclets` argument or the “roclets” option
  ([`load_options()`](https://roxygen2.r-lib.org/dev/reference/load_options.md)).
  It defaults to using the Collate “roclet”, the Rd roclet, and
  NAMESPACE roclet, but you can also add your own.

- Runs the different methods of all those roclets, in order and
  independently: clean (`roclet_clean`), preprocess
  ([`roclet_preprocess()`](https://roxygen2.r-lib.org/dev/reference/roclet.md)),
  process
  ([`roclet_process()`](https://roxygen2.r-lib.org/dev/reference/roclet.md)),
  and output
  ([`roclet_output()`](https://roxygen2.r-lib.org/dev/reference/roclet.md)).
  Only process and output are routinely used. For example, if you think
  of the Rd roclet, its process method digests information from the tag
  parsing, combines inherits, etc. to create the content of each
  documentation topic, and its output method writes those topics to
  disk.

## Key data structures

Before we dive into extending roxygen2, we need to first discuss two
important data structures that power roxygen: tags and blocks.

### Tags

A tag (a list with S3 class `roxy_tag`) represents a single tag. It has
the following fields:

- `tag`: the name of the tag.

- `raw`: the raw contents of the tag (i.e. everything from the end of
  this tag to the beginning of the next).

- `val`: the parsed value, which we’ll come back to shortly.

- `file` and `line`: the location of the tag in the package. Used with
  [`roxy_tag_warning()`](https://roxygen2.r-lib.org/dev/reference/roxy_tag.md)
  to produce informative error messages.

You *can* construct tag objects by hand with
[`roxy_tag()`](https://roxygen2.r-lib.org/dev/reference/roxy_tag.md):

``` r
roxy_tag("name", "Hadley")
#> [????:???] @name 'Hadley' {unparsed}
str(roxy_tag("name", "Hadley"))
#> List of 5
#>  $ file: chr NA
#>  $ line: chr NA
#>  $ raw : chr "Hadley"
#>  $ tag : chr "name"
#>  $ val : NULL
#>  - attr(*, "class")= chr [1:2] "roxy_tag_name" "roxy_tag"
```

However, you should rarely need to do so (except in tests), because
you’ll typically have them given to you in a block object, as you’ll see
shortly.

### Blocks

A block (a list with S3 class `roxy_block`) represents a single roxygen
block. It has the following fields:

- `tags`: a list of `roxy_tags`.
- `call`: the R code associated with the block (usually a function
  call).
- `file` and `line`: the location of the R code.
- `object`: the evaluated R object associated with the code.

The easiest way to see the basic structure of a
[`roxy_block()`](https://roxygen2.r-lib.org/dev/reference/roxy_block.md)
is to generate one by parsing a roxygen block with
[`parse_text()`](https://roxygen2.r-lib.org/dev/reference/parse_package.md):

``` r
text <- "
  #' This is a title
  #'
  #' This is the description.
  #'
  #' @param x,y A number
  #' @export
  f <- function(x, y) x + y
"

# parse_text() returns a list of blocks, so I extract the first
block <- parse_text(text)[[1]]
block
#> <roxy_block> [<text>:8]
#>   $tag
#>     [line:  2] @title 'This is a title' {parsed}
#>     [line:  4] @description 'This is the description.' {parsed}
#>     [line:  6] @param 'x,y A number' {parsed}
#>     [line:  7] @export '' {parsed}
#>     [line:  8] @usage '<generated>' {parsed}
#>     [line:  8] @.formals '<generated>' {parsed}
#>     [line:  8] @backref '<generated>' {parsed}
#>   $call   f <- function(x, y) x + y
#>   $object <function> 
#>     $topic f
#>     $alias f
```

You’ll notice that some of the tags didn’t exist in the original block:

- `@title` and `@description` are extracted from the text that appears
  before the first explicit tag.
- `@usage` is generated automatically from the function formals.
- `@.formals` is an “internal” tag that doesn’t generate any output but
  is used to pass some important data around.
- `@backref` stores the source location of the block so we can later
  record which `.R` files contributed to each `.Rd` file.

## Adding a new `.Rd` tag

The most common way to extend roxygen2 is to create a new tag that adds
output to `.Rd` files. This requires defining a few methods:

1.  Define a
    [`roxy_tag_parse()`](https://roxygen2.r-lib.org/dev/reference/roxy_tag.md)
    method that describes how to parse our new tag.

2.  Define a
    [`roxy_tag_rd()`](https://roxygen2.r-lib.org/dev/reference/roxy_tag_rd.md)
    method that describes how to convert the tag into `.Rd` commands.

3.  If the tag’s content is meant to appear in a custom section (as
    opposed to, say, the examples section), define a
    [`format()`](https://rdrr.io/r/base/format.html) method that
    describes how to create the `.Rd` string.

To illustrate the basic idea, we’ll create a new `@tip` tag that will
create a bulleted list of tips about how to use a function. The idea is
to take something like this:

``` r
#' @tip The mean of a logical vector is the proportion of `TRUE` values.
#' @tip You can compute means of dates and date-times!
```

That generates Rd like this:

``` latex
\section{Tips and tricks}{
\itemize{
  \item The mean of a logical vector is the proportion of \code{TRUE} values.
  \item You can compute means of dates and date-times!
}
}
```

The first step is to define a method for
[`roxy_tag_parse()`](https://roxygen2.r-lib.org/dev/reference/roxy_tag.md)
that describes how to parse the tag text. The name of the class will be
`roxy_tag_{tag}`, which in this case is `roxy_tag_tip`. This function
takes a `roxy_tag` as input, and its job is to set `x$val` to a
convenient parsed value that will be used later by the roclet. Here we
want to process the text using Markdown so we can just use
[`tag_markdown()`](https://roxygen2.r-lib.org/dev/reference/tag_parsers.md):

``` r
roxy_tag_parse.roxy_tag_tip <- function(x) {
  tag_markdown(x)
}
```

(There are lots of other built in options that you can read about in
[`?tag_markdown`](https://roxygen2.r-lib.org/dev/reference/tag_parsers.md).)

We can check this works by using
[`parse_text()`](https://roxygen2.r-lib.org/dev/reference/parse_package.md):

``` r
text <- "
  #' Title
  #'
  #' @tip The mean of a logical vector is the proportion of `TRUE` values.
  #' @tip You can compute means of dates and date-times!
  #' @md
  f <- function(x, y) {
    # ...
  }
"
block <- parse_text(text)[[1]]
block
#> <roxy_block> [<text>:7]
#>   $tag
#>     [line:  2] @title 'Title' {parsed}
#>     [line:  4] @tip 'The mean of a logical vector is the proportion ...' {parsed}
#>     [line:  5] @tip 'You can compute means of dates and date-times!' {parsed}
#>     [line:  6] @md '' {parsed}
#>     [line:  7] @usage '<generated>' {parsed}
#>     [line:  7] @.formals '<generated>' {parsed}
#>     [line:  7] @backref '<generated>' {parsed}
#>   $call   f <- function(x, y) { ...
#>   $object <function> 
#>     $topic f
#>     $alias f

str(block$tags[[2]])
#> List of 5
#>  $ file: chr "<text>"
#>  $ line: int 4
#>  $ tag : chr "tip"
#>  $ raw : chr "The mean of a logical vector is the proportion of `TRUE` values."
#>  $ val : chr "The mean of a logical vector is the proportion of \\code{TRUE} values."
#>  - attr(*, "class")= chr [1:2] "roxy_tag_tip" "roxy_tag"
```

Here I explicitly turn Markdown parsing on using `@md`; it’s usually
turned on for a package using roxygen2’s
[`load_options()`](https://roxygen2.r-lib.org/dev/reference/load_options.md).

Next, we define a method for
[`roxy_tag_rd()`](https://roxygen2.r-lib.org/dev/reference/roxy_tag_rd.md),
which must create an
[`rd_section()`](https://roxygen2.r-lib.org/dev/reference/rd_section.md).
We’re going to create a new custom section called `tip`. It will contain
a character vector of tips:

``` r
roxy_tag_rd.roxy_tag_tip <- function(x, base_path, env) {
  rd_section("tip", x$val)
}
```

This additional layer is needed because there can be multiple tags of
the same type in a single block, and multiple blocks can contribute to
the same `.Rd` file. The job of
[`rd_section()`](https://roxygen2.r-lib.org/dev/reference/rd_section.md)
is to combine all the tags into a single top-level Rd section. Each tag
generates an `rd_section` which is then combined with any previous
section using [`merge()`](https://rdrr.io/r/base/merge.html). The
default `merge.rd_section()` just concatenates the values together
(`rd_section(x$type, c(x$value, y$value))`); you can override this
method if you need more sophisticated behaviour.

We called the custom section “tip” just like our tag, but we needn’t
have done so: it really depends on how you want to map the input tags to
output Rd code.

We then need to define a
[`format()`](https://rdrr.io/r/base/format.html) method to convert this
object into text for the `.Rd` file:

``` r
format.rd_section_tip <- function(x, ...) {
  paste0(
    "\\section{Tips and tricks}{\n",
    "\\itemize{\n",
    paste0("  \\item ", x$value, "\n", collapse = ""),
    "}\n",
    "}\n"
  )
}
```

We can now try this out with `roclet_text()`:

``` r
topic <- roc_proc_text(rd_roclet(), text)[[1]]
topic$get_section("tip")
#> \section{Tips and tricks}{
#> \itemize{
#>   \item The mean of a logical vector is the proportion of \code{TRUE} values.
#>   \item You can compute means of dates and date-times!
#> }
#> }
#> 
```

Note that there is no namespacing so if you’re defining multiple new
tags I recommend using your package name as the common prefix.

### Using your new tag

Now that the three methods are created, we still need to make them
available to
[`roxygenize()`](https://roxygen2.r-lib.org/dev/reference/roxygenize.md).
First, you need to export the method so that it’s registered correctly.
Since your methods will be for generics defined in roxygen2 (another
package), you’ll need to follow the advice in [Methods for generics in
other
packages](https://roxygen2.r-lib.org/dev/articles/rd-S3.html#methods-for-generics-in-other-packages).

Next, you’ll need to load the tag-defining package in the package where
you want to use it. For example, if you created some new tags in
{packageFoo}, and would like to use these tags in your documentation for
{packageBar}, append this line to the `DESCRIPTION` of {packageBar}:

    Config/roxygen2/packages: packageFoo

See
[`load_options()`](https://roxygen2.r-lib.org/dev/reference/load_options.md)
for more details.

## Creating a new roclet

Creating a new roclet is usually a two part process. First, you define
new tags that your roclet will work with, unless your roclet only needs
information from existing tags, or only needs the path to the package
source[¹](#fn1). Second, you define a roclet that tells
[`roxygenize()`](https://roxygen2.r-lib.org/dev/reference/roxygenize.md)
what to compute and produce based on this information.

### Custom tags

In this example we will make a new `@memo` tag which helps you to
remember what you’re planning to work on in the future by displaying
notes when you document your package. We choose this syntax for `@memo`:

``` r
#' @memo [Headline] Description
```

For example:

``` r
#' @memo [EFFICIENCY] Currently brute-force; find better algorithm.
```

As above, we first define a parse method. This time we use custom format
based on a regular expression:

``` r
roxy_tag_parse.roxy_tag_memo <- function(x) {
  if (!grepl("^\\[.*\\].*$", x$raw)) {
    roxy_tag_warning(x, "Invalid memo format")
    return()
  }

  parsed <- regmatches(x$raw, regexec("\\[(.*)\\](.*)", x$raw))[[1]]

  x$val <- list(
    header = parsed[[2]],
    message = parsed[[3]]
  )
  x
}
```

Then we check it works with
[`parse_text()`](https://roxygen2.r-lib.org/dev/reference/parse_package.md):

``` r
text <- "
  #' @memo [TBI] Remember to implement this!
  #' @memo [API] Check best API
  f <- function(x, y) {
    # ...
  }
"
block <- parse_text(text)[[1]]
block
#> <roxy_block> [<text>:4]
#>   $tag
#>     [line:  2] @memo '[TBI] Remember to implement this!' {parsed}
#>     [line:  3] @memo '[API] Check best API' {parsed}
#>     [line:  4] @usage '<generated>' {parsed}
#>     [line:  4] @.formals '<generated>' {parsed}
#>     [line:  4] @backref '<generated>' {parsed}
#>   $call   f <- function(x, y) { ...
#>   $object <function> 
#>     $topic f
#>     $alias f

str(block$tags[[1]])
#> List of 5
#>  $ file: chr "<text>"
#>  $ line: int 2
#>  $ tag : chr "memo"
#>  $ raw : chr "[TBI] Remember to implement this!"
#>  $ val :List of 2
#>   ..$ header : chr "TBI"
#>   ..$ message: chr " Remember to implement this!"
#>  - attr(*, "class")= chr [1:2] "roxy_tag_memo" "roxy_tag"
```

We don’t need a format method because our tag won’t be used to produce
Rd sections[²](#fn2).

### The roclet

Next, we create a constructor for the roclet, which uses
[`roclet()`](https://roxygen2.r-lib.org/dev/reference/roclet.md). Our
`memo` roclet doesn’t have any options so this is very simple:

``` r
memo_roclet <- function() {
  roclet("memo")
}
```

To give the roclet behaviour, you need to define methods. There are two
methods that almost every roclet will use:

- [`roclet_process()`](https://roxygen2.r-lib.org/dev/reference/roclet.md)
  is called with a list of blocks, and returns an object of your
  choosing.

- [`roclet_output()`](https://roxygen2.r-lib.org/dev/reference/roclet.md)
  produces side-effects (usually writing to disk) using the result from
  [`roclet_process()`](https://roxygen2.r-lib.org/dev/reference/roclet.md).

For this roclet, we’ll have
[`roclet_process()`](https://roxygen2.r-lib.org/dev/reference/roclet.md)
collect all the memo tags into a named list:

``` r
roclet_process.roclet_memo <- function(x, blocks, env, base_path) {
  results <- list()

  for (block in blocks) {
    tags <- block_get_tags(block, "memo")

    for (tag in tags) {
      msg <- paste0("[", tag$file, ":", tag$line, "] ", tag$val$message)
      results[[tag$val$header]] <- c(results[[tag$val$header]], msg)
    }
  }

  results
}
```

And then have
[`roclet_output()`](https://roxygen2.r-lib.org/dev/reference/roclet.md)
print them to the screen:

``` r
roclet_output.roclet_memo <- function(x, results, base_path, ...) {
  for (header in names(results)) {
    messages <- results[[header]]
    cat(paste0(header, ": ", "\n"))
    cat(paste0(" * ", messages, "\n", collapse = ""))
  }

  invisible(NULL)
}
```

Then you can test if it works by using
[`roc_proc_text()`](https://roxygen2.r-lib.org/dev/reference/roc_proc_text.md):

``` r
results <- roc_proc_text(
  memo_roclet(),
  "
#' @memo [TBI] Remember to implement this!
#' @memo [API] Check best API
f <- function(x, y) {
  # ...
}

#' @memo [API] Consider passing z option
g <- function(x, y) {
  # ...
}
"
)
roclet_output(memo_roclet(), results)
#> TBI: 
#>  * [<text>:2]  Remember to implement this!
#> API: 
#>  * [<text>:3]  Check best API
#>  * [<text>:8]  Consider passing z option
```

## Adding a roclet to your workflow

To use a roclet when developing a package, call

``` r
roxygen2::roxygenize(roclets = "yourPackage::roclet")
```

where `yourPackage::roclet` is the function which creates the roclet,
e.g. `memo_roclet` above.

You can also add the roclet to the target package’s DESCRIPTION file,
like this:

``` r
Config/roxygen2/roclets: collate, rd, namespace, yourPackage::roclet
```

See
[`load_options()`](https://roxygen2.r-lib.org/dev/reference/load_options.md)
for more details.

Your package only needs to be installed when the user documents the
package, which doesn’t correspond precisely to any field in the
`DESCRIPTION`. However, you can think of it as a development dependency
and hence put it in the `Suggests:` field:

``` r
usethis::use_package("yourPackage", type = "Suggests")
```

You don’t have to do this, but it will help other developers working on
the target package.

## Conclusion: further extension ideas

This vignette is quite rough, so you might want to also read some
roxygen2 source code to understand all the extension points.

Do not hesitate to also look for examples of roxygen2 extensions. For
instance, the [roxygenlabs](https://github.com/gaborcsardi/roxygenlabs)
package (former incubator of roxygen2 features) used a third extension
point: it *extended* the Rd roclet with further methods, thus creating a
supercharged Rd roclet rather than a brand-new roclet. Or, the
[plumber2](https://github.com/posit-dev/plumber2) package only uses the
parsing features from roxygen2, and does not use
[`roxygenize()`](https://roxygen2.r-lib.org/dev/reference/roxygenize.md)
at all.

------------------------------------------------------------------------

1.  For example, the no-longer recommended
    [`vignette_roclet()`](https://roxygen2.r-lib.org/dev/reference/vignette_roclet.md)
    only needs the path to the package source as input; it does not use
    information from the tag parsing step. Or the {roxylint} package
    only uses existing tags; its job is to warn you about suboptimal
    roxygen2 style.

2.  Some tags in roxygen2 itself, like `@importFrom`, are not meant for
    the
    \[[`rd_roclet()`](https://roxygen2.r-lib.org/dev/reference/rd_roclet.md)\]
    (`@importFrom` is meant for the
    [`namespace_roclet()`](https://roxygen2.r-lib.org/dev/reference/namespace_roclet.md)).
