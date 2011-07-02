# A v. simple api for dealing with lists of tags in a rd file

new_rd_file <- function() {
  structure(list(environment()), class = "rd_file")
}
is.rd_file <- function(x) inherits(x, "rd_file")

#' @S3method print rd_file
print.rd_file <- function(x, ...) {
  tags <- vapply(as.list(x[[1]]), "[[", "tag", FUN.VALUE = character(1))
  cat("Rd file with tags ", str_c(tags, collapse = ", "), "\n", sep = "")
}

#' @S3method format rd_file
format.rd_file <- function(x, ...) {
  tags <- as.list(x[[1]])
  order <- c("docType", "name", "alias", "title", "usage", "arguments",
    "value", "description", "details", "note", "section", "examples",
    "author", "references", "seealso", "concept", "keyword")
    
  tags <- tags[intersect(order, names(tags))]
  
  str_c(unlist(lapply(tags, format)), collapse = "")
}

#' @S3method length rd_file
length.rd_file <- function(x) {
  length(x[[1]])
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