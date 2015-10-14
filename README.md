# roxygen2

[![Build Status](https://travis-ci.org/klutometis/roxygen.png)](https://travis-ci.org/klutometis/roxygen)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/roxygen2)](http://cran.r-project.org/package=roxygen2)
[![Coverage Status](https://img.shields.io/codecov/c/github/klutometis/roxygen/master.svg)](https://codecov.io/github/klutometis/roxygen?branch=master)

> all' hileth', Hephaiste; didou d'areten te kai olbon.*
> --Homer, 7th century BCE

# Why use roxygen2?

The premise of `roxygen2` is simple: describe your functions in comments next to their definitions and `roxygen2` will process your source code and comments to produce Rd files in the `man/` directory.  Here's a simple example from the `stringr` package:

```R
#' The length of a string (in characters).
#'
#' @param string input character vector
#' @return numeric vector giving number of characters in each element of the
#'   character vector.  Missing strings have missing length.
#' @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' str_length(letters)
#' str_length(c("i", "like", "programming", NA))
str_length <- function(string) {
  string <- check_string(string)

  nc <- nchar(string, allowNA = TRUE)
  is.na(nc) <- is.na(string)
  nc
}
```

When you `roxygenise` your package these comments will be automatically transformed to the Rd file you need to pass `R CMD check`:

```
\name{str_length}
\alias{str_length}
\title{The length of a string (in characters).}
\usage{str_length(string)}
\arguments{
  \item{string}{input character vector}
}
\description{
The length of a string (in characters).
}
\seealso{\code{\link{nchar}} which this function wraps}
\value{
  numeric vector giving number of characters in each element of the
  character vector.  Missings string have missing length.
}
\examples{
str_length(letters)
str_length(c("i", "like", "programming", NA))
}
```

# Installation

To get the current released version from CRAN:

```R
install.packages("roxygen2")
```

To get the current development version from github:

```R
# install.packages("devtools")
devtools::install_github("klutometis/roxygen")
```

# Running

Roxygen does a live analysis of your source code: it loads all the code in your package, so it can create documentation using values in an R environment, not just source code. However, simulating package loading is rather tricky to do in general, so there are two ways to do it with roxygen:

* `roxygen2::roxygenise()` just sources all files in the `R/` directory

* `devtools::document()` sources all files in the `R/` directory, compiles
  source code in the `src/` directory, loads data in the `data/` directory
  and generally does an accurate job of simulating package loading.

If you have a simple package, you can use `roxygenise()`, but for anything more complicated, I recommend that you use `document()`.

# Roclets

`roxygen2` comes with four roclets, tools for parsing your source code and producing files useful for documenting your package:

* `collate_roclet`: allows you to add `@include` directives to ensure that
  files are loaded in the order they are needed

* `namespace_roclet`: creates your `NAMESPACE` automatically. 95% of the time
  all you need to do is label functions, methods and classes that you want to
  export with the `@export` tag

* `rd_roclet`: produces Rd files by inspecting both function definitions and
  roxygen2 comments in the source code

* `vignette_roclet`: builds vignettes using `tools::buildVignette()`.

By default, `roxygenise` will run the first three, but you can choose which ones to run using the `roclet` parameter, or field `Roxygen` in your `DESCRIPTION`:

```
Roxygen: list(roclets = c("rd", "collate"))
```

-----------
* Hail, Hephaistos! Grant skill and weal.
