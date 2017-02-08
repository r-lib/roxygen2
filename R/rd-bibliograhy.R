
#' @import bibtex
NULL

## Handling of bibliography-related tags
#' @export 
#' @rdname roxy_tag 
tag_cite <- function(x){
  # convert into @references \cite{<entry1>} \cite{<entry2>} ... 
  x <- tag_words()(x)
  roxy_tag('references', val = sprintf("\\cite{%s}", x$val))
}

#' @export 
#' @rdname roxy_tag 
tag_bibliography <- function(x){
  # add path to bib file to current BibObject handler
  bib <- RoxyBibObject()
  x <- tag_words()(x)
  bib$add_bibfile(x$val, block = x)
  NULL
}


# substitutes \cite commands with short or long citation
#' @importFrom digest digest
process_cite <- function(block, base_path, env, global_options){

  # get bibliography handler (cached)
  BIBS <- RoxyBibObject()
  
  tags_cite <- global_options$cite_tags %||% c('introduction', 'description', 'details', 'section', 'param')
  # backup original block value
  block0 <- block
  
  # 1. process all tags that can have \cite commands
  j_cite <- which(names(block) %in% tags_cite)
  if( length(j_cite) ){
    cite_res <- lapply(block[j_cite], gsub_cite, bibs = BIBS, short = TRUE, block = block)
    block[j_cite] <- lapply(cite_res, '[[', 'value')
    bibkeys <- unique(unlist(lapply(cite_res, '[[', 'bibkeys')))
    
    # 2. add parsed keys as references tags
    if( length(bibkeys) ){
      bibkeys <- unique(bibkeys)
      lapply(bibkeys, function(bk){
        block <<- append(block, list(references = sprintf('\\cite{%s}', bk)))
      })
    }
  }
  
  # 3. process references
  j_ref <- which(names(block) %in% 'references')
  if( length(j_ref) ){
    ref_res <- lapply(block[j_ref], gsub_cite, bibs = BIBS, short = FALSE, block = block)
    # process references as markdown
    block[j_ref] <- lapply(ref_res, function(x) tag_markdown(roxy_tag('references', val = x$value))$val)
  }
  
  # update in parsed block only if necessary
  if( digest(block) != digest(block0) ) return(block)
  block0
}

# find cite tags and resolve them against bibfiles
gsub_cite <- function(tag, bibs, short = TRUE, block = NULL){
  
  # cope for different types of tags
  field <- intersect(names(tag), c('value', 'val'))
  if( length(field) ){
    field <- field[1L]
    x <- tag[[field]]
    
  }else x <- tag
  
  # extract \cite tags
  cite_match <- str_match_all(x, "\\\\cite\\{([^}]+)\\}")
  # for each process citations
  res <- list(value = x, bibkeys = NULL)
  
  lapply(seq_along(cite_match), function(i){
        m <- cite_match[[i]]
        # no \cite command: return string untouched
        if( !length(m) ) return()
        
        # split into individual bibkeys
        keys <- strsplit(m[, 2L], ';')
        # process each command
        mapply(function(cite_s, key){
              key <- str_trim(key)
              res$bibkeys <<- union(res$bibkeys, key)
              fkey <- bibs$format_cite(key, short = short, block = block)
              res$value[i] <<- gsub(cite_s, paste(fkey, collapse = if( short ) ', ' else "\n\n"), res$value[i], fixed = TRUE)
            }, m[, 1L], keys)
      })
  
  if( length(field) ){
    tag[[field]] <- res$value
    res$value <- tag
  }
  
  res
}

## Biobliography handler
RoxyBibObject <- local({
      .obj <- NULL
      function(base_path = NA, reset = FALSE){
        if( reset ) .obj <<- NULL
        # create or update instance
        if( is.null(.obj) ) .obj <<- RoxyBib$new(base_path)
        else .obj$set_path(base_path)
        .obj
      }
    })

RoxyBib <- R6::R6Class("RoxyTopic", public = list(
        
        # data members
        base_path = NA,
        bibfiles = character(),
        bibs_loaded = character(),
        bibs = list(),
        bibentries = list(),
        
        # constructor
        initialize = function(path = NA) {
          self$set_path(path)
        },
        
        set_path = function(path = NA){
          if( is.na(path) ) return()
          self$base_path <- path
          ref_file <- file.path(self$base_path, 'inst/REFERENCES.bib')
          # append file to set of bibfiles if it exists
          if( file.exists(ref_file) ) self$add_bibfile(ref_file, prepend = TRUE)
        },
        
        add_bibfile = function(path, check = TRUE, block = NULL, prepend = FALSE){
          if( check && !file.exists(path) ) block_warning(block, "could not find bibliograpy file ", path)
          npath <- normalizePath(path)
          self$bibfiles <- union(self$bibfiles, npath)
          if( prepend ) self$bibfiles <- union(npath, self$bibfiles)
          npath
        },
        
        load_bib = function(){
          path <- setdiff(self$bibfiles, self$bibs_loaded)[1L]
          if( is.na(path) ) return(FALSE)
          library(bibtex)
          newbibs <- read.bib2(file = path)
          n <- length(self$bibs)
          self$bibs <- if( !length(self$bibs) ) newbibs else c(self$bibs, newbibs[setdiff(names(newbibs), names(self$bibs))])
          message(sprintf("Loaded %i new bibentry from %s", length(self$bibs) - n, path))
          self$bibs_loaded <- c(self$bibs_loaded, path)
          TRUE
        },
        
        # write package REFERENCES.bib file
        update_bibfile = function(file = NULL){
          if( !length(self$bibentries) ) return()
          file <- file %||% file.path(self$base_path, 'inst/REFERENCES.bib')
          message(sprintf("Writing file %s", basename(file)))
          write.bib(self$bibentries, file = file)
          file
        },
        
        # fetch bibitem from key
        get_bib = function(key, block = NULL){
          
          hit <- setNames(rep(NA_integer_, length(key)), key)
          while( anyNA(hit) ){
            bibkeys <- names(self$bibs)
            hit[key] <- match(key, bibkeys)
            if( !self$load_bib() ) break
          }
          
          if( anyNA(hit) ){
            msg <- sprintf("Could not find bib entry for key(s) %s", paste(names(hit)[is.na(hit)], collapse = ', '))
            if( !is.null(block) ) block_warning(block, msg)
            else warning(msg)
          }
          
          self$bibs[names(hit)[!is.na(hit)]]
        },
        
        format_cite = function(key, short = TRUE, ...){
          # load bibitem
          res <- setNames(key, key)
          bibitems <- self$get_bib(key, ...)
          if( !length(bibitems) ) return(res)
          
          # add bibitems to set of used bibitems for final output in package REFERENCES.bib
          if( !length(self$bibentries) ) self$bibentries <- bibitems
          else self$bibentries <- c(self$bibentries, bibitems[setdiff(names(bibitems), names(self$bibentries))])
          
          # format accordingly
          if( !short ){
            res[names(bibitems)] <- format(bibitems)
            res
          }else{
            res[names(bibitems)] <- sapply(bibitems, function(x){
                  if( length(x$author$family) <= 1L ) 
                    paste(x$author$family, ' (', x$year, ')', sep='')				
                  else{
                    paste(x$author$family[[1]], ' et al. (', x$year, ')', sep='')
                  }
                })
            res
          }
        }
    
    ))

# copied and fixed from bibtex::read.bib
read.bib2 <- function (file = findBibFile(package), package = "bibtex", encoding = "unknown", 
    header = if (length(preamble)) paste(preamble, sep = "\n") else "", 
    footer = "") 
{
  if (!is.character(file)) {
    stop("'read.bib' only supports reading from files, 'file' should be a character vector of length one")
  }
  srcfile <- switch(encoding, unknown = srcfile(file), srcfile(file, 
          encoding = encoding))
  out <- .External("do_read_bib", file = file, encoding = encoding, 
      srcfile = srcfile)
  at <- attributes(out)
  if ((typeof(out) != "integer") || (getRversion() < "3.0.0")) 
    out <- lapply(out, make.bib.entry)
  else out <- list()
  preamble <- at[["preamble"]]
  out <- make.citation.list(out, header, footer)
  attr(out, "strings") <- at[["strings"]]
  # keys must be retrieved here to handle the case of skipped bibentries
  keys <- sapply(out, function(x) attr(x, 'key') %||% x$key)
  names(out) <- keys
  out
}
environment(read.bib2) <- asNamespace('bibtex')
