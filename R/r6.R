# Extract all methods from an RC definition, returning a list of "objects".
r6_methods <- function(obj) {
  stopifnot(methods::is(obj, "R6ClassGenerator"))

  methods_public <- obj$public_methods
  methods_active <- obj$active

  ## of the public methods, the initialize method will be renamed to "new"
  if(any(names(methods_public) == "initialize")) {
      names(methods_public)[which(names(methods_public) == "initialize")] <- "new"
  }
  
  ## add the metadata
  for(i in seq_along(methods_public)) {
      methods_public[[i]] <- add_r6_metadata(methods_public[[i]], name=names(methods_public)[i], class=class(obj), is_active=FALSE)
  }
  for(i in seq_along(methods_active)) {
      methods_active[[i]] <- add_r6_metadata(methods_active[[i]], name=names(methods_active)[i], class=class(obj), is_active=TRUE)
  }

  
  methods_obj <- lapply(c(methods_public, methods_active), object)
  return(methods_obj)
}

add_r6_metadata <- function(val, name, class, is_active) {
  class(val) <- c("r6method", "function")
  attr(val, "r6class") <- class
  attr(val, "r6method") <- name
  attr(val, "r6is_active") <- is_active

  val
}



# Modified from docstring - a function has a doc block
# if it's a call to {, with more than 1 element, and the first element is
# a character vector.
docblock <- function(f) {
  stopifnot(is.function(f))
  if (is.primitive(f)) return(NULL)

  b <- body(f)
  if (length(b) <= 2 || !identical(b[[1]], quote(`{`))) return(NULL)

  first <- b[[2]]
  if (!is.character(first)) return(NULL)

  ## now get all the elements that are character vectors until there are no more
  element_is_character <- unlist(lapply(b, is.character))
  if(!all(element_is_character[-1])) {
      last_block_line <- min(which(!element_is_character[-1]))
  }
  else {
      last_block_line <- length(b)
  }

  ## now we need some more processing
  ## first standardize so that no empty lines exist
  block <- unlist(as.list(b)[2:last_block_line])

  ## make it into one character string, then split on empty lines into sub-blocks
  block_one_line <- paste(block, collapse="\n")
  ## Strip off trailing and leading blank lines:
  block_one_line <- gsub("^\n+|\n+$", "", block_one_line)

  subblocks <- strsplit(block_one_line, split="\n\\s*\n[\n\\s]*", perl=TRUE, fixed=FALSE)[[1]]
  
  param_lines <- unlist(lapply(subblocks, extract_param_from_block))
  blocks_nonparam <- lapply(subblocks, extract_nonparam_from_block)
  ## remove empty blocks
  blocks_nonparam <- blocks_nonparam[unlist(lapply(blocks_nonparam, length)) > 0]
  
  ## if there are any param_lines, make a describe block
  if(length(param_lines) > 0) {
      param_beginning <- "\\strong{Parameters:}\n\\describe{"
      param_ending <- "}"
      item_lines <- unlist(lapply(param_lines, paramline_to_item))
      param_block <- paste(c(param_beginning, item_lines, param_ending), collapse="\n")
  }
  else {
      param_block <- character(0)
  }

  return(paste(c(blocks_nonparam, param_block), collapse="\n\n"))
}

## assumption is that block has no empty lines
extract_param_from_block <- function(block) {
    block_split <- strsplit(block, split="[\\s\n]*@param\\s+", perl=TRUE, fixed=FALSE)[[1]]
    if(length(block_split) == 1) {
        return(NULL)
    }
    else {
        return(block_split[-1])
    }
}

## assumption is that block has no empty lines
extract_nonparam_from_block <- function(block) {
    block_nonparam <- strsplit(block, split="[\\s\n]*@param\\s+", perl=TRUE, fixed=FALSE)[[1]][1]
    if(block_nonparam=="") {
        return(NULL)
    }
    else {
        return(block_nonparam)
    }
}

paramline_to_item <- function(line) {
    ## strip starting whitespace
    line <- gsub("^\\s+", "", line, perl=TRUE)

    ## split off the first word
    first_space <- regexpr("\\s", line, perl=TRUE)
    if(first_space==-1) { # no hit
        first_word <- line
        remaining <- ""
    }
    else {
        first_space_len <- attr(first_space, "match.length")
        first_word <- substr(line, start=1, stop=first_space-1)
        remaining <- substr(line, start=first_space + first_space_len, stop=nchar(line))
    }
       
    ## check that remaining is rdComplete
    if(!rdComplete(first_word)) {
        stop(paste("Not Rd complete:", first_word))
    }
    if(!rdComplete(remaining)) {
        stop(paste("Not Rd complete:", remaining))
    }
    return(paste0("\\item{", first_word, "}{", remaining, "}"))
}
