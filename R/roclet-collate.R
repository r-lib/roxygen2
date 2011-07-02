#' @include parse.R
NULL

register.preref.parsers(parse.value, 'include')

#' Roclet: make Collate field in DESCRIPTION.
#'
#' Topologically sort R files and record in Collate field.
#'
#' Each \code{@@include} tag should specify the filename of one intrapackage
#' dependency; multiple \code{@@include} tags may be given.
#'
#' @return Rd roclet
#' @examples
#' #' `example-a.R', `example-b.R' and `example-c.R' reside
#' #' in the `example' directory, with dependencies
#' #' a -> {b, c}. This is `example-a.R'.
#' #' @@include example-b.R
#' #' @@include example-c.R
#' NULL
#'
#' roclet <- collate_roclet()
#' \dontrun{
#'   roc_proc(roclet, dir('example'))
#'   roc_out(roclet, dir('example'), "example")
#' }
#' @export
collate_roclet <- function() {
  new_roclet(list(), "collate")
}

#' @S3method roc_process collate
roc_process.collate <- function(roclet, partita, base_path) {
  vertices <- make_vertices()
  
  for (partitum in partita) {
    file <- base_path(partitum$srcref$filename, base_path)
    vertex <- vertices$add(file)
    
    includes <- partitum[names(partitum) == "include"]
    if (length(includes) > 0) {
      for (include in includes) {
        vertices$add_ancestor(vertex, include)
      }
    }
  }

  sorted <- vertices$topological_sort()
  unique(basename(sapply(sorted, function(x) x$file)))
}
                                  
#' @S3method roc_output collate
roc_output.collate <- function(roclet, results, base_path) {
  DESCRIPTION <- file.path(base_path, "DESCRIPTION")
  old <- read.description(DESCRIPTION)
  new <- old
  new$Collate <- str_c("'", results, "'", collapse = " ")
  write.description(new, DESCRIPTION)
  
  if (!identical(old, read.description(DESCRIPTION))) {
    cat('Updating collate directive in ', DESCRIPTION, "\n")
  }    
}

base_path <- function(path, base) {
  path <- normalizePath(path)
  base <- normalizePath(base)
  
  str_replace(path, fixed(str_c(base, "/")), "")
}

make_vertices <- function() {
  vertices <- NULL

  make.vertex <- function(file) {
    vertex <- new.env(parent = emptyenv())
    vertex$file <- file
    vertex$discovered <- FALSE
    vertex$ancestors <- NULL
    vertex
  }

  maybe.append.vertex <- function(file) {
    if (is.null(vertices[[file]])) {
      vertices[[file]] <<- make.vertex(file)
    }
    vertices[[file]]
  }

  member <- function(ancestor, ancestors) {
    for (vertex in ancestors)
      if (identical(ancestor, vertex))
        TRUE
    FALSE
  }
  
  maybe.append.ancestor <- function(predecessor, ancestor_name) {
    ancestor <- maybe.append.vertex(ancestor_name)
    
    if (!member(ancestor, predecessor$ancestors)) {
      predecessor$ancestors <- append(ancestor, predecessor$ancestors)
    }
  }


  topological.sort <- function() {
    sorted <- NULL
    visit <- function(predecessor) {
      predecessor$discovered <- TRUE
      for (ancestor in predecessor$ancestors) {
        if (!ancestor$discovered) {
          visit(ancestor)
        }
      }
      sorted <<- append(sorted, predecessor)
    }
    
    for (vertex in vertices) {
      if (!vertex$discovered) {
        visit(vertex)        
      }
    }

    sorted
  }
  
  list(add = maybe.append.vertex, 
     add_ancestor = maybe.append.ancestor, 
     topological_sort = topological.sort
  )
}
