
#' A funny generic
#'
#' @s4methods
setGeneric(
    name="G1",
    def=function (a1,a2,a3,...){
        standardGeneric("G1")
    }
)

#' A title 
#'
#' @param a1 The only parameter described
setMethod(f="G1",
  signature=signature(
    a1="numeric",
    a2="character"
  ),
  definition=function (a1,a2,a3) {
     return()
  }
)

#' Only a title no params described
#'
setMethod(f="G1",
  signature=signature(
    a1="numeric"
  ),
  definition=function(a1,a2,a3){
     return()
  }
)

#' Only a title no params described
#'
setMethod(f="G1",
  signature=signature(
    a1="logical"
  ),
  definition=function(a1,a2,...){
     return()
  }
)
