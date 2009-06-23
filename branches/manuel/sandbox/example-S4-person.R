# S4 documentation using Roxygen.



#' This class represents a person.
#' @slot fullname The full name of the person
#' @slot birthyear The year of birth
#' @export
setClass('Person',
         representation=
         representation(fullname='character',
                        birthyear='numeric'),
         prototype=
         prototype(fullname='John Doe',
                   birthyear=1947),
         contains='test')

#' Constructor function for Person object.
#' @param fullname The name of the person.
#' @param birthyear The year of birth.
#' @return The Person object
#' @export
Person <- function(fullname, birthyear) {
  return(new('Person', fullname=fullname, birthyear=birthyear))
}

#' The naming of an object.
#'
#' @param object A object which gets a name
setGeneric('name', function(object, y, ...) standardGeneric('name'), valueClass='character')

#' Name a person, the baptism.
#'
#' @param object A Person object
#' @export
setMethod('name', signature=signature(object='Person', y='numeric'),
function(object, y, ...) {
  return(object@fullname)
})

#' Blub a person.
#'
#' @param object A Person object
#' @export
setMethod('blub', signature=signature(object='Person', y='character'),
function(object, y, ...) {
  return(object@fullname)
})


