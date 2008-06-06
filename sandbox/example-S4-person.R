# An example for S4 documentation using Roxygen.



#' This class represents a person.
#'
#' @slot fullname The full name of the person
#' @slot birthyear The year of birth
#' @prototype Prototype person is named John Doe
#'      and born in the year 1971
setClass('Person',
         representation=
         representation(fullname='character',
                        birthyear='numeric'),
         prototype=
         prototype(fullname='John Doe',
                   birthyear=1971))

#' Constructor function for Person object.
#' @param fullname The name of the person.
#' @param birthyear The year of birth.
#' @return Person The Person object
#' @export
Person <- function(fullname, birthyear) {
  return(new('Person', fullname=fullname, birthyear=birthyear))
}

#' The naming of an object.
#'
#' @param object A object which gets a name
setGeneric('name', function(object) standardGeneric('name'))

#' Name a person, the baptism.
#'
#' @param object A Person object
#' @export
setMethod('name', 'Person',
function(object) {
  return(object@fullname)
})

