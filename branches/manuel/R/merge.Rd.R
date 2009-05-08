
merge.Rd <- function(x, y, ...) {
  
  nlTag.Rd <- function()
    return(list(structure('\n', Rd_tag='TEXT')))

  mymerge.default <- function(x, y) {
    attr <- attributes(x)
    
    x <- c(x, nlTag.Rd(), y)
    attributes(x) <- attr
    
    return(x)
  }

  mymerge.arguments <- function(x, y) {
    attr <- attributes(x)
    
    xitems <- unlist(sapply(x, '[[', 1))
    yitems <- unlist(sapply(y, '[[', 1))

    for ( i in setdiff(yitems, xitems) )
      x <- c(x, list(structure(y[[which(i == yitems)]], Rd_tag='\\item')))

    attributes(x) <- attr
    
    return(x)
  }
    
  
  MULTIPLE_TAGS <- c('\\keyword', '\\alias')
  IGNORE_IF_AVAILABLE_TAGS <- c('\\name', '\\description', '\\author',
                                '\\title')
  
  
  ytags <- tools:::RdTags(y)
  iytags <- which(ytags != 'TEXT')

  x <- unclass(x)
  xtags <- tools:::RdTags(x)
  
  for ( i in iytags ) {
    tag <- ytags[i]

    if ( tag %in% xtags ) {
      if ( !(tag %in% IGNORE_IF_AVAILABLE_TAGS) ) {
        if ( tag %in% MULTIPLE_TAGS ) {
          x <- c(x, y[i])
        }
        else {
          j <- which(xtags == tag)

          if ( tag == '\\arguments' )
            x[[j]] <- mymerge.arguments(x[[j]], y[[i]])
          else
            x[[j]] <- mymerge.default(x[[j]], y[[i]])
        }
      }
    }
    else {
      x <- c(x, y[i])
    }
  }
  
  return(structure(x, class='Rd'))
}

