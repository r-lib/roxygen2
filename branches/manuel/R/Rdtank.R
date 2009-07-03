
make.Rdtank <- function() {

  tank <- new.env(parent=emptyenv())

  tank$documents <- list()
  tank$mergelist <- list()
  tank$classmethods <- list()
  tank$classlist <- list()
  tank$methods <- list()

  tank$add.Rd <- function(rd, name, filename=NULL) {
    tank$documents[[name]] <- rd
    if ( !is.null(filename) )
      tank$mergelist[[filename]] <-
        c(tank$mergelist[[filename]], name)
    
    invisible(NULL)
  }

  tank$update.Rd <- function(rd, name=NULL, classname=NULL) {
    if ( !is.null(name) )
      tank$documents[[name]] <- rd
    if ( !is.null(classname) )
      tank$documents[[tank$classlist[[classname]]]] <- rd
  }

  tank$get.Rd.by <- function(name=NULL, filename=NULL, classname=NULL) {
    if ( !is.null(name) )
      return(tank$documents[name])
    if ( !is.null(filename) )
      return(tank$documents[tank$mergelist[[filename]]])
    if ( !is.null(classname) )
      return(tank$documents[tank$classlist[[classname]]])
  }
  
  tank$register.S4class <- function(classname, name)
    tank$classlist[[classname]] <- name

  tank$register.S4method <- function(generic, name, signature, description) {
    for ( class in signature )
      tank$classmethods[[class]] <-
        c(tank$classmethods[[class]],
          list(list(generic=generic, name=name,
                    signature=signature, description=description)))
    
    tank$methods[[generic]] <-
      c(tank$methods[[generic]], list(list(name=name,
                                           signature=signature,
                                           description=description)))
  }
  
  tank$filenames <- function()
    names(tank$mergelist)

  tank$classnames <- function()
    names(tank$classmethods)

  tank$generics <- function()
    names(tank$methods)
  
  tank$class.exists <- function(class)
    !is.null(tank$documents[[class]])

  tank$get.class.methods <- function(class)
    tank$classmethods[[class]]

  tank$get.methods <- function(generic)
    tank$methods[[generic]]
  
  tank$reset <- function() {
    tank$documents <- list()
    tank$mergelist <- list()
    tank$classmethods <- list()
    tank$classes <- list()
  }

  tank
}

