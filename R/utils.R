"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

# Given argument list, produce usage string for it.
# 
# Adapted from \code{\link{prompt}}.
#
# @param f function, or name of function, as string
# @return a string
usage <- function(args) {
  is.missing.arg <- function(arg) {
    is.symbol(arg) && deparse(arg) == ""
  }
  arg_to_text <- function(arg) {
    if (is.missing.arg(arg)) return("")
    text <- deparse(arg, backtick = TRUE, width.cutoff = 500L)
    text <- str_replace_all(text, fixed("%"), "\\%")
    text <- str_replace_all(text, fixed(" "), "\u{A0}")
    Encoding(text) <- "UTF-8"    
    
    str_c("\u{A0}=\u{A0}", paste(text, collapse = "\n"))
  }

  arg_values <- vapply(args, arg_to_text, character(1))
  
  paste(names(args), arg_values, collapse = ", ", sep = "")
}

# Does the string contain no matter, but very well [:space:]?
# @param string the string to check
# @return TRUE if the string contains words, otherwise FALSE
is.null.string <- function(string) {
  str_length(str_trim(string)) == 0
}


subs <- matrix(ncol = 2, byrow = T, c(
  '[]', 'sub',
  '<-', 'set',
  '!', 'not',
  '"', 'quote',
  '#', 'hash',
  '$', 'cash',
  '%', 'grapes',
  '&', 'and',
  '|', 'or',
  "'", 'single-quote',
  '(', 'open-paren',
  ')', 'close-paren',
  '*', 'star',
  '+', 'plus',
  ',', 'comma',
  '/', 'slash',
  ':', 'colon',
  ';', 'semi-colon',
  '<', 'less-than',
  '=', 'equals',
  '>', 'greater-than',
  '?', 'p',
  '@', 'at',
  '[', 'open-brace',
  '\\', 'backslash',
  ']', 'close-brace',
  '^', 'hat',
  '`', 'tick',
  '{', 'open-curly',
  '}', 'close',
  '~', 'twiddle'
))
subs[, 2] <- str_c("-", subs[, 2])

nice_name <- function(x) {
  for(i in seq_len(nrow(subs))) {
    x <- str_replace_all(x, fixed(subs[i, 1]), subs[i, 2])
  }
  x <- str_replace(x, "-+", "-")
  x
}


roxygen_stop <- function(..., srcref = NULL) {
  stop(..., srcref_location(srcref), call. = FALSE)
}

roxygen_warning <- function(..., srcref = NULL) {
  warning(..., srcref_location(srcref), call. = FALSE)
}

srcref_location <- function(srcref = NULL) {
  if (is.null(srcref)) return()
  str_c(" in block ", basename(srcref$filename), ":", srcref$lloc[1])
}


# From the bibtex package dev version
write.bib <- function(entry=NULL, file="Rpackages.bib", append = FALSE, verbose = TRUE)
{
	# special handling of file=NULL: use stdout()
	if( is.null(file) ){
		file <- stdout()
		verbose <- FALSE
	}	
	## use all installed packages if nothing is specified
	if( is.null(entry) ){ 
		if( verbose ) message("Generating Bibtex entries for all installed packages ", appendLF=FALSE)
		entry <- unique(installed.packages()[,1])
		if( verbose ) message("[", length(entry), "]")
	}
	
	bibs <- 
			if( is(entry, 'bibentry') )	entry
			else if( is.character(entry) ){
				if( length(entry) == 0 ){
					if( verbose ) message("Empty package list: nothing to be done.")
					return(invisible())
				}
				
				pkgs <- entry
				bibs <- sapply(pkgs, function(x) try(citation(x)), simplify=FALSE)
				#bibs <- lapply(pkgs, function(x) try(toBibtex(citation(x))))
				n.installed <- length(bibs)
				
				## omit failed citation calls
				ok <- sapply(bibs, is, 'bibentry')
				pkgs <- pkgs[ok]
				bibs <- bibs[ok]
				n.converted <- sum(ok)
				
				## add bibtex keys to each entry
				pkgs <- lapply(seq_along(pkgs), function(i) if(length(bibs[[i]]) > 1)
								paste(pkgs[i], c('', 2:length(bibs[[i]])), sep = "") else pkgs[i])
				pkgs <- do.call("c", pkgs)
				bibs <- do.call("c", bibs)		
				# formatting function for bibtex keys:
				# names with special characters must be enclosed in {}, others not.
				as.bibkey <- function(x){
					i <- grep("[.]", x)
					if( length(i) > 0 )
						x[i] <- paste("{", x[i], "}", sep='')
					x
				}		
				bibs <- mapply(function(b,k){ if( is.null(b$key) ) b$key <- k; b}, bibs, pkgs, SIMPLIFY=FALSE)
				bibs <- do.call("c", bibs)
				
				if(verbose) message("Converted ", n.converted, " of ", n.installed, " package citations to BibTeX")					
				bibs
			} else
				stop("Invalid argument `entry`: expected a bibentry object or a character vector of package names.")
	
	if( length(bibs) == 0 ){
		if( verbose ) message("Empty bibentry list: nothing to be done.")
		return(invisible())
	}
	
	## write everything to the .bib file
	fh <- if( is.character(file) ){
				if( !grepl("\\.bib$", file) ) # add .bib extension if necessary 
					file <- paste(file, '.bib', sep='')
				fh <- file(file, open = if(append) "a+" else "w+" )
				on.exit( if( isOpen(fh) ) close(fh) )
				fh
			} else if( is(file, 'connection') )
				file
			else
				stop("Invalid argument `file`: expected a filename, NULL, or a connection [", class(file), "]")
	
	if( !is(fh, 'connection') )
		stop("Invalid connection: ", fh)		
	file.desc <- summary(fh)['description']
	
	if( verbose ) message(if( append ) "Adding " else "Writing ", length(bibs) , " Bibtex entries ... ", appendLF=FALSE)
	writeLines(toBibtex(bibs), fh)
	if(verbose) message("OK\nResults written to file '", file.desc, "'")
	
	## return Bibtex items invisibly
	invisible(bibs)
}
