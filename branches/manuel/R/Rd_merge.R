
Rd_merge <- function(rdlist, base=Rd(), mergefn=merge.Rd) { 
  for ( rd in names(rdlist) )
    base <- mergefn(base, rdlist[[rd]], yname=rd)

  return(list(base))
}

merge.Rd <- function(x, y, yname=NULL, mergers=simplemergers(), ...) {
  # NOTE: x is the base rd.

  getMerger <- function(name)
    if ( !is.null(mergers[[name]]) ) mergers[[name]] else mergers$DEFAULT

  
  MULTIPLE <- c('\\alias', '\\keyword')
    
  xtagnames <- tools:::RdTags(x)
  ytagnames <- tools:::RdTags(y)  
  
  for ( yat in which(ytagnames != 'TEXT') ) {
    ytagname <- ytagnames[yat]
    ytag <- y[[yat]]
    
    xat <- NULL
    
    if ( !(ytagname %in% MULTIPLE) ) {  
      if ( ytagname %in% xtagnames )
        xat <- which(xtagnames == ytagname)

      merger <- getMerger(substring(ytagname, 2))
      ytag <- merger(if (is.null(xat)) NULL else x[[xat]], ytag, yname)
    }
    
    x <- Rd_append_tag(x, ytag, xat)
  }

  return(x)
}

simplemergers <- function() {
  return(list(DEFAULT=default.merger,
              name=one.merger,
              description=one.merger,
              author=one.merger,
              title=one.merger,
              value=paragraph.merger,
              details=paragraph.merger,
              arguments=arguments.merger))
}

default.merger <- function(x, y, yname) {
  if ( is.null(x) )
    return(y)

  return(Rd_tag_append_tag(x, y, newline=FALSE))
}

one.merger <- function(x, y, yname) {
  if ( is.null(x) )
    return(y)
  
  return(x)
}

arguments.merger <- function(x, y, name) {
  if ( is.null(x) )
    return(y)
  
  attr <- attributes(x)
    
  xitems <- unlist(sapply(x, '[[', 1))
  yitems <- unlist(sapply(y, '[[', 1))
  
  for ( i in setdiff(yitems, xitems) )
    x <- Rd_tag_append_tag(x, y[which(i == yitems)])

  attributes(x) <- attr
  
  return(x)
}

paragraph.merger <- function(x, y, yname) {
  t <- textTag(sprintf('\\code{%s}: ', yname))
  attr <- attributes(y)
  y <- c(t, y, newlineTag(), newlineTag())
  attributes(y) <- attr
  
  if ( is.null(x) )
    return(y)

  return(Rd_tag_append_tag(x, y))  
}

