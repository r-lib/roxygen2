# A v. simple api for dealing with lists of tags in a rd file

new_rd_file <- function() {
  structure(list(environment()), class = "rd_file")
}
is.rd_file <- function(x) inherits(x, "rd_file")

#' @S3method print rd_file
print.rd_file <- function(x, ...) {
  cat("Rd file with tags ", str_c(names(x), collapse = ", "), "\n", sep = "")
}

#' @S3method names rd_file
names.rd_file <- function(x) {
  vapply(as.list(x[[1]]), "[[", "tag", FUN.VALUE = character(1))
}


#' @S3method format rd_file
format.rd_file <- function(x, ...) {
  tags <- as.list(x[[1]])
  order <- c("docType", "encoding", "name", "alias", "title", "format",
    "source", "usage", "arguments", "value", "description", "details", "slot",
    "note", "section", "examples", "author", "references", "seealso",
    "concept", "keyword")
    
  tags <- tags[intersect(order, names(tags))]
  
  formatted <- lapply(tags, "format")
  str_c(unlist(formatted), collapse = "")
}

#' @S3method merge rd_file
merge.rd_file <- function(x, y, ...) {
  rd <- new_rd_file()
  for(tag_x in as.list(x[[1]])) {
    add_tag(rd, tag_x)
  }
  for(tag_y in as.list(y[[1]])) {
    add_tag(rd, tag_y)
  }
  rd
}

#' @S3method length rd_file
length.rd_file <- function(x) {
  length(x[[1]])
}

get_tag <- function(file, tagname) {
  file[[1]][[tagname]]
}

add_tag <- function(file, tag) {
  if (is.null(tag)) return()
  stopifnot(is.rd_file(file))
  
  if (!is.rd_tag(tag) && is.list(tag)) {
    return(lapply(tag, add_tag, file = file))
  }
  stopifnot(is.rd_tag(tag))
  
  existing <- file[[1]][[tag$tag]]
  if (is.null(existing)) {
    file[[1]][[tag$tag]] <- tag 
  } else {
    file[[1]][[tag$tag]] <- merge(existing, tag)[[1]]
  }

  invisible()
}
