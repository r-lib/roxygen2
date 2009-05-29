

### Rd tag elements:

nameTag <- function(x) {
  return(Rd_tag(verbTag(x), '\\name'))
}

aliasTag <- function(x) {
  return(Rd_tag(verbTag(x), '\\alias'))
}

keywordTag <- function(x) {
  return(Rd_tag(textTag(x), '\\keyword'))
}

detailsTag <- function(..., x=list(...)) {
  .totext <- function(t) {
    if ( is(t, 'Rd_tag') ) t else textTag(t)
  }

  return(Rd_tag(lapply(x, .totext), '\\details'))
}

itemTag <- function(x, y=NULL) {
  if ( is.null(y) )
    y <- x[[2]]; x <- x[[1]]
  
  return(Rd_tag(list(list(textTag(x)),
                     list(textTag(y))), '\\item'))
}

argumentsTag <- function(..., x=list(...), newline=TRUE) {
  if ( newline )
    x <- newlineSeperators(x)
    
  return(Rd_tag(x, '\\arguments'))
}

methodTag <- function(x, y) {
  return(Rd_tag(list(list(textTag(x)),
                     list(textTag(y))), '\\method'))
}

usageTag <- function(x, y, newline=TRUE) {
  if ( newline )
    x <- newlineSeperators(x)
  
  return(Rd_tag(list(x, rcodeTag(sprintf('(%s)', y))), '\\usage'))
}

newlineTag <- function() {
  return(textTag('\n'))
}

newlineSeperators <- function(x) {
  l <- 2 * length(x)
  
  t <- vector('list', length=l)
  t[seq(1, l, by=2)] <- x
  t[seq(2, l, by=2)] <- newlineTag()
  
  return(t)
}


### Basic tag elements:

Rd_tag <- function(x, tag) {
  UseMethod('Rd_tag')
}
  
Rd_tag.default <- function(x, tag) {
  return(structure(as.character(x), Rd_tag=tag, class='Rd_tag'))
}

Rd_tag.Rd_tag <- function(x, tag) {
  return(structure(list(x), Rd_tag=tag, class='Rd_tag'))
}

Rd_tag.list <- function(x, tag) {
  return(structure(x, Rd_tag=tag, class='Rd_tag'))
}
 
verbTag <- function(x) {
  return(Rd_tag(x, 'VERB'))
}

textTag <- function(x) {
  return(Rd_tag(x, 'TEXT'))
}

rcodeTag <- function(x) {
  return(Rd_tag(x, 'RCODE'))
}

Rd_tag_append_tag <- function(tag1, tag2, newline=TRUE) {

  attr <- attributes(tag1)
  tag1 <- c(tag1, tag2)
  attributes(tag1) <- attr

  if ( newline )
    tag1 <- Rd_tag_append_tag(tag1, newlineTag(), newline=FALSE)
 
  return(tag1)
}



### Rd functions:

Rd_append_tag <- function(rd, tag, at=NULL, newline=TRUE) {
  if ( is.null(at) )
    at <- length(rd) + 1
  
  rd[[at]] <- tag

  if ( newline ) 
    rd[[at+1]] <- newlineTag()
  
  return(rd)
}



### Rd element:

Rd <- function() {
  return(structure(list(), class='Rd'))
}
