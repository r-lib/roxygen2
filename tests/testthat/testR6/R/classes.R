#' Class A
#'
#' @description
#' Class A description.
#'
#' @details
#' Class A details
#'
#' @param fogbehindme This is not show up in the output at all.
#' @param Z zzzzzzz

A <- R6::R6Class(
  "A",
  public = list(
    #' @field field1 A field 1.
    field1 = NULL,
    #' @description A method 1.
    #' @examples
    #' ## Example for meth1
    meth1 = function(Z) {},
    #' @description Method 2 description.
    #' @details Method 2 details.
    #' @param Z Overriding Z argument for meth2.
    #' @param ... Rest.
    #' @examples
    #' ## Example for meth2
    meth2 = function(Z = 10, ...) {},
    #' @field field2 A field 2.
    field2 = "foobar",
    #' @details Method 3 details.
    #' @param duplicate This one is
    #' @param duplicate Twice
    #' @return twice.
    #' @return really?
    meth3 = function(duplicate, missing) {},
    #' @field field3 A field 3.
    field3 = "baz"
  ),
  active = list(
    #' @field active1 A binding 1.
    active1 = function(x) {},
    #' @field active2 A binding 2.
    active2 = function(x) {},
    #' @field active3 A binding 2.
    active3 = function(x) {}
  )
)

#' Class B
#'
#' @description
#' Class B Description.
#'
#' @details
#' Class B details.

B <- R6::R6Class(
  "B",
  inherit = A,
  public = list(
    #' @field field1 B field 1.
    field1 = NULL,
    #' @field field4 B field 4.
    field4 = NULL,
    #' @description B method 1.
    #' @param Z Still zzzzzzzz.
    meth1 = function(Z) {},
    #' @description A method 4.
    meth4 = function() {}
  ),
  active = list(
    #' @field active1 B binding 1.
    active1 = function(x) {},
    #' @field active4 B binding 4.
    active4 = function(x) {},
    #' @field active5 B binding 5.
    active5 = function(x) {}
  )
)

#' Class C
#'
#' @description
#' Class C Description.
#'
#' @details
#' Classs C details.
#'
#' @field nosuchfield This will warn.

C <- R6::R6Class(
  "C",
  inherit = B,
  cloneable = FALSE,
  public = list(
    field2 = NULL,
    #' @description C method 2.
    #' @param Z zzzzz
    #' @param ... etc
    meth2 = function(Z = 10, ...) {},
    #' @field field5 C field 5.
    field5 = "foobar",
    #' @description C method 5.
    meth5 = function() {},
    undocumented_field = NULL,
    undocumented_method = function() {},
    #' @field duplicatefield Multiple.
    #' @field duplicatefield times.
    duplicatefield = NULL
  ),
  active = list(
    #' @field active2 C binding 2.
    active2 = function(x) {},
    #' @field active4 C binding 4.
    active4 = function(x) {},
    #' @field active6 C binding 6.
    active6 = function(x) {},
    undocumented_binding = function(x) {},
    #' @field duplicate_binding Double.
    #' @field duplicate_binding Double double.
    duplicate_binding = function(x) {}
  )
)
