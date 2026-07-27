# Documenting S4

There are three things that you might document for
[S4](https://adv-r.hadley.nz/s4.html):

- **Generics**: document as functions; mention that they are generics.
- **Methods**: unlike S3/S7, all S4 methods must be documented.
- **Classes**: document the class definition and use `@slot` for slots.

## Generics

S4 **generics** are functions, so document them as such. `@export` a
generic if you want users to call it or other developers to write
methods for it. If the generic is internal, you don’t need to export or
document it.

### Supported syntax

- `setGeneric("balance", function(x) standardGeneric("balance"))`

- `setGroupGeneric("Ops2", function(e1, e2) ...)`, which defines a group
  generic and is otherwise documented just like `setGeneric()`.

## Classes

Document **S4 classes** by adding a roxygen block before `setClass()`.
`@export` a class if you want users to create instances or other
developers to extend it (e.g. by creating subclasses). Internal classes
don’t need documentation. Use `@slot` to document the slots of the
class. Here’s a simple example:

``` r

#' An S4 class to represent a bank account
#'
#' @slot balance A length-one numeric vector
#' @export
Account <- setClass("Account", slots = list(balance = "numeric"))
```

### Supported syntax

- `setClass("Account", slots = list(balance = "numeric"))`

- `setClassUnion("Number", c("numeric", "integer"))`, which defines a
  class and is otherwise documented just like `setClass()`.

## Methods

S4 **methods** are a little more complicated. Unlike S3 and S7 methods,
all S4 methods must be documented. You only need to `@export` a method
if the generic lives in another package.

You can document methods in three places:

- In the class. Most appropriate if the corresponding generic uses
  single dispatch and you created the class.

- In the generic. Most appropriate if the generic uses multiple
  dispatches and you control it.

- In its own file. Most appropriate if the method is complex or the
  either two options don’t apply.

Use either `@rdname` or `@describeIn` to control where method
documentation goes. See
[`vignette("reuse")`](https://roxygen2.r-lib.org/dev/articles/reuse.md)
for more details.

### Supported syntax

- `setMethod("show", "Account", function(object) ...)`

- `setReplaceMethod("balance", "Account", function(x, value) ...)`,
  which documents the `balance<-` method, aliased as
  `balance<-,Account-method`.

- `setAs("numeric", "Account", function(from) ...)`, which documents the
  `coerce()` method. The usage comes from `coerce()`, so must you
  document `@param to` and `@param strict` even if your definition only
  takes `from`.

  If you supply `replace`, only the `coerce()` method is documented; use
  `@aliases` if you also want to document `coerce<-`.
