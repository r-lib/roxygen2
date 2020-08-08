
#################### define generics

#' getName
#'
#' @s4methods
setGeneric(
    name='getName',
    def=function(obj){
      standardGeneric('getName')
    }
)

#' title
#'
#' @s4methods
setGeneric(
    name='getSallary',
    def=function(obj){
      standardGeneric('getSallary')
    }
)

#################### define classes

#' title
#'
#' @s4subclasses
#' @s4superclasses
#' @s4methods
setClass(
   Class="Person",
   slots=c(name="character")
)

#' title
#'
#' @s4subclasses
#' @s4superclasses
#' @s4methods
setClass(
   Class="Customer",
   contains=c("Person"),
   slots=c(nr="numeric",orders="numeric")
)

#' title
#'
#' @s4subclasses
#' @s4superclasses
#' @s4methods
setClass(
   Class="Employe",
   contains=c("Person"),
   slots=c(sallary="numeric")
)

#' title
#'
#' @s4subclasses
#' @s4superclasses
#' @s4methods
setClass(
   Class="EmployedCustomer",
   contains=c("Employe","Customer"),
   slots=c(discounts="character")
)

#################### define methods

#' title
#'
setMethod(
    f='getName',
    signature=signature(obj="Person"),
    definition=function(obj){
      obj@name
    }
)

#' title
#'
setMethod(
    f='getSallary',
    signature=signature(obj="Employe"),
    definition=function(obj){
      obj@sallary
    }
)
