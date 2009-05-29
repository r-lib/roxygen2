
Rd_merge <- function(x, y, appenders=simpleappenders(), mergers=simplemergers()) {
  # x is the base Rd, all tags from y are merged into x.

  getMerger <- function(name)
    if ( !is.null(mergers[[name]]) ) mergers[[name]] else mergers$DEFAULT

  getAppender <- function(name)
    if ( !is.null(appenders[[name]]) ) appenders[[name]] else appenders$DEFAULT

  
  yname <- unlist(y[[which(sapply(y, attr, 'Rd_tag') == '\\alias')[1]]])
  
  MULTIPLE <- c('\\alias', '\\keyword')
  
  xtagnames <- tools:::RdTags(x)
  ytagnames <- tools:::RdTags(y)  

  for ( yat in which(ytagnames != 'TEXT') ) {
    ytagname <- ytagnames[yat]
    ytag <- y[[yat]]
    
    xat <- NULL

    if ( !(ytagname %in% MULTIPLE) ) {      
      if ( ytagname %in% xtagnames ) {
        xat <- which(xtagnames == ytagname)
        merger <- getMerger(substring(ytagname, 2))
        
        ytag <- merger(x[[xat]], ytag, yname)
      }
    }
    
    #x <- Rd_append_tag(x, ytag, xat)
    appender <- getAppender(substring(ytagname, 2))
    x <- appender(x, ytag, xat, yname)
  }

  return(x)
}



### Appender:

simpleappenders <- function() {
  return(list(DEFAULT=default.appender,
              value=value.appender))
}

default.appender <- function(x, y, at, name) {
  return(Rd_append_tag(x, y, at))
}

value.appender <- function(x, y, at, name) {
  x <- Rd_tag_append_tag(x, textTag(sprintf('\\code{%s}:', name)))
  return(Rd_append_tag(x, y))
}



### Merger:

simplemergers <- function() {
  return(list(DEFAULT=default.merger,
              name=omity.merger,
              description=omity.merger,
              author=omity.merger,
              title=omity.merger,
              value=value.merger,
              arguments=arguments.merger))
}

default.merger <- function(x, y, name) {
  return(Rd_tag_append_tag(x, y))
}

omity.merger <- function(x, y, name) {
  return(x)
}

arguments.merger <- function(x, y, name) {
  attr <- attributes(x)
    
  xitems <- unlist(sapply(x, '[[', 1))
  yitems <- unlist(sapply(y, '[[', 1))
  
  for ( i in setdiff(yitems, xitems) )
    x <- Rd_tag_append_tag(x, y[which(i == yitems)])

  attributes(x) <- attr
  
  return(x)
}

value.merger <- function(x, y, name) {
  return(Rd_tag_append_tag(x, y))
}
