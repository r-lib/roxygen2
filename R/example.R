#' This class represents a person.
#'
#' @slot fullname The full name of the person
#' @slot birthyear The year of birth
#' @prototype Prototype person is named John Doe
#'      and born in the year 1971}
setClass('Person',
         representation = representation (
                     fullname = 'character',
                     birthyear = 'numeric') ,
         prototype = prototype (
                     fullname = 'John Doe',
                     birthyear = '1971' ))
#' The naming of an object.
#'
#' @param object A object which gets a name
setGeneric('name', function(object, ...){})
#' Name a person, the baptism.
#'
#' @param object A Person object
#' @param ... Not used
#' @export
setMethod('name', signature('Person'),
function(object, ...) {
  ...
})
