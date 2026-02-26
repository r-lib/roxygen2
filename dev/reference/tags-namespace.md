# Tags for managing the `NAMESPACE`

Learn the full details in
[`vignette('namespace')`](https://roxygen2.r-lib.org/dev/articles/namespace.md).

Key tags:

- `@export`: Export this function, method, generic, or class so it's
  available outside of the package.

- `@exportS3Method ${1:package}::${2:generic}`: Export an S3 method.
  Only needed when the method is for a generic from a suggested package.

- `@importFrom ${1:package} ${2:function}`: Import specific functions
  from a package.

- `@useDynLib ${1:package}`: Import compiled code from another package.

Other less frequently used tags:

- `@evalNamespace ${1:r-code}`: Evaluate arbitrary code in the package
  namespace and insert the results into the `NAMESPACE`. Should return a
  character vector of directives.

- `@exportClass ${1:class}`: Export an S4 class. For expert use only; in
  most cases you should use `@export` so roxygen2 can automatically
  generate the correct directive.

- `@exportMethod ${1:generic}`: Export S4 methods. For expert use only;
  in most cases you should use `@export` so roxygen2 can automatically
  generate the correct directive.

- `@exportPattern ${1:pattern}`: Export all objects matching a regular
  expression.

- `@import ${1:package}`: Import all functions from a package. Use with
  extreme care.

- `@importClassesFrom ${1:package} ${2:class}`: Import S4 classes from
  another package.

- `@importMethodsFrom ${1:package} ${2:generic}`: Import S4 methods from
  a package.

- `@rawNamespace ${1:namespace directives}`: Insert literal text
  directly into the `NAMESPACE`.

## Usage

``` r
#' @evalNamespace ${1:r-code}
#' @export
#' @exportClass ${1:class}
#' @exportMethod ${1:generic}
#' @exportPattern ${1:pattern}
#' @exportS3Method ${1:package}::${2:generic}
#' @import ${1:package}
#' @importClassesFrom ${1:package} ${2:class}
#' @importFrom ${1:package} ${2:function}
#' @importMethodsFrom ${1:package} ${2:generic}
#' @rawNamespace ${1:namespace directives}
#' @useDynLib ${1:package}
```
