# A v. simple api for dealing with lists of tags in a rd file

new_rd_file <- function() {
  env <- new.env(parent = emptyenv())
  structure(list(env), class = "rd_file")
}
is.rd_file <- function(x) inherits(x, "rd_file")

copy_env <- function(x) {
  list2env(as.list(x), parent = emptyenv())
}

#' @export
print.rd_file <- function(x, ...) {
  cat("Rd file with tags ", paste0(names(x), collapse = ", "), "\n", sep = "")
}

#' @export
names.rd_file <- function(x) {
  vapply(as.list(x[[1]]), "[[", "tag", FUN.VALUE = character(1))
}

#' @export
format.rd_file <- function(x, ...) {
  tags <- as.list(x[[1]])
  order <- c("backref", "docType", "encoding", "name", "alias", "title",
    "format", "source", "usage", "param", "value", "description",
    "details", "minidesc", "reexport", "field", "slot", "rcmethods", "note",
    "section", "examples", "author", "references", "seealso",
    "concept", "keyword", "rawRd")

  tags <- tags[intersect(order, names(tags))]

  formatted <- lapply(tags, "format", ...)

  paste0(made_by("%"), paste0(unlist(formatted), collapse = ""))
}

#' @export
merge.rd_file <- function(x, y, ...) {
  if (is.null(x))
    return(y)
  if (is.null(y))
    return(x)

  for(tag_y in as.list(y[[1]])) {
    add_tag(x, tag_y)
  }
  x
}

#' @export
length.rd_file <- function(x) {
  length(x[[1]])
}

get_tag <- function(file, tagname) {
  file[[1]][[tagname]]
}

add_tag <- function(file, tag, overwrite = FALSE) {
  if (is.null(tag)) return()
  stopifnot(is.rd_file(file))

  if (!is.rd_tag(tag) && is.list(tag)) {
    return(lapply(tag, add_tag, file = file, overwrite = overwrite))
  }
  stopifnot(is.rd_tag(tag))

  existing <- file[[1]][[tag$tag]]
  if (is.null(existing) || overwrite) {
    file[[1]][[tag$tag]] <- tag
  } else {
    file[[1]][[tag$tag]] <- merge(existing, tag)[[1]]
  }

  invisible()
}
