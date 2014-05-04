#' @include roclet-rd.R
NULL

#' Roclet: make Rd files from Markdown-Roxygen comments.
#' 
#' Supports a limited subset of Markdown syntax in Roxygen comments, see Details.
#' 
#' Supported Markdown mark-up: \itemize{ \item itemized lists (lines starting
#' with a star followed by a blank, subsequent lines for the same item starting
#' with 4 spaces) \item enumerated lists (first item starting with 1 followed by
#' a period, subsequent items starting with numbers+period or star, or 4 spaces
#' to continue same item)\item \code{code}-formatting with backticks
#' (\code{`<code>`}) \item strong/bold (\code{**<bold>**}) \item 
#' italic/emphasized text (\code{*<emph>*}) \item links to other function docs 
#' (\code{![<function>](<some_package>)} or \code{![function]}) \item inline
#' math (\code{$<math>$}) \item displayed equations (\code{$$<math>$$})}.
#' @seealso \code{tests/test-md2rd.R} for syntax examples.
#' 
#' @family roclets
#' @export
md2rd_roclet <- function() {
  new_roclet(list(), "md2rd")
}

md2rd <- function(md){
# replaces Markdown syntax with Rd-Syntax 
  
  # bold:
  # look for star star (no star, no blank) (..., no star)  (no blank, star star)
  rd <- str_replace_all(md, 
                        pattern = 
                          "(\\*\\*)([^\\*[:blank:]][^\\*]+[^[:blank:]])(\\*\\*)", 
                        replacement = "\\\\bold{\\2}")
  
  # italics:
  # look for star (no star, no blank) (..., no star)  (no blank, star)
  rd <- str_replace_all(rd, 
                        pattern = 
                          "(\\*)([^\\*[:blank:]][^\\*]+[^[:blank:]])(\\*)",
                        replacement = "\\\\emph{\\2}")
  
  #displayed math:
  # look for $$ (no "$")  $$:
  rd <-  str_replace_all(rd, 
                         pattern = "(\\$\\$)([^$\n]+)(\\$\\$)", 
                         replacement = "\\\\deqn{\\2}")
  
  # inline math:
  # look for $ (no "$")  $:
  rd <- str_replace_all(rd,
                        pattern = "(\\$)([^$\n]+)(\\$)", 
                        replacement = "\\\\eqn{\\2}")
  
  # code:
  # look for ` (no "`") `:
  rd <- str_replace_all(rd,
                        pattern = "(\\`)([^`\n]+)(\\`)", 
                        replacement = "\\\\code{\\2}")
  
  
  # enums:
  # look for newline 1. blank(s) [...] (newline (star OR number. OR 4 blanks) ....)
  enumpattern <- 
    "\\n1\\.[[:blank:]]+[[:print:]]+(\\n[*([:digit:]+.)[:blank:]{4}][[:print:]]+)+"
  while(!any(is.na(enumposition <- str_locate(rd, enumpattern)))){
    str <- str_extract(rd, enumpattern)
    str <- str_replace_all(str, 
                           pattern = "\\n\\*", 
                           replacement = "\\\n\\\\item")
    str <- str_replace_all(str, 
                           pattern = "\\n[0-9]+\\.", 
                           replacement = "\\\n\\\\item")
    str <- paste("\n\\enumerate{", str, "}")
    str_sub(rd, enumposition[1, 1], enumposition[1, 2]) <- str
  }
  
  # lists:
  # look for newline star blank(s) [...] (newline (star OR 4 blanks) ....)
  itempattern <- 
    "\\n[*][[:blank:]]+[[:print:]]+(\\n[*[:blank:]{4}][[:print:]]+)+"
  while(!any(is.na(itemposition <- str_locate(rd, itempattern)))){
    str <- str_extract(rd, itempattern)
    str <- str_replace_all(str, 
                           pattern = "\\n\\*", 
                           replacement = "\\\n\\\\item")
    str <- paste("\n\\itemize{", str, "}")
    str_sub(rd, itemposition[1, 1], itemposition[1, 2]) <- str
  }
  
  # links
  #look for ![ (no "]") ]( (no")") ) and ![ (no "]") ] 
  rd <- str_replace_all(rd,
                        pattern = "\\!\\[([^]]+)\\]\\(([^)]+)\\)", 
                        replacement = "\\\\link[\\2]{\\1}")
  rd <- str_replace_all(rd,
                        pattern = "\\!\\[([^]]+)\\]", 
                        replacement = "\\\\link{\\1}")
  
  rd
}



#' @export
roc_process.md2rd <- function(roclet, parsed, base_path, options = list()) {
  parsed$blocks <- rapply(parsed$blocks, 
                          f = md2rd,
                          classes = "character",
                          how = "replace")
  parsed$blocks <- lapply(parsed$blocks, FUN = 
                            function(block) {
                              if(!is.null(block$object)) {
                                # re-instate "pseudo-function"-class so 
                                # default_usage() dispatches to correct method.
                                class(block$object) <- c("function", "object")
                              }
                              block
                            })
  
  call <- match.call()
  call[[1]] <- roc_process.had
  call$roclet <- rd_roclet()
  eval(call)
}


#' @export
roc_output.md2rd <- roc_output.had


#' @export
clean.md2rd <- clean.had