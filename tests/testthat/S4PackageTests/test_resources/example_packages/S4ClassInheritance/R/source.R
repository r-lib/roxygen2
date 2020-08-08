
#' title
#'
#' @s4subclasses
#' @s4superclasses
setClass(
   Class="Person",
   slots=c(name="character")
)

#' title
#'
#' @s4subclasses
#' @s4superclasses
setClass(
   Class="Customer",
   contains=c("Person"),
   slots=c(nr="numeric",orders="numeric")
)

#' title
#'
#' @s4subclasses
#' @s4superclasses
setClass(
   Class="Employe",
   contains=c("Person"),
   slots=c(sallary="numeric")
)

#' title
#'
#' @s4subclasses
#' @s4superclasses
setClass(
   Class="EmployedCustomer",
   contains=c("Employe","Customer"),
   slots=c(discounts="character")
)
