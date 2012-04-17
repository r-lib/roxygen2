# Definition of the roclet that handles @cite and @bibliography tags
# 
# Author: Renaud Gaujoux
# Creation: 17 Apr 2012
###############################################################################

# parser for @bibliography tags: store filenames in global variable 'bibfiles'
register.preref.parser('bibliography', function(key, name, srcref) {
		file <- parse.value(key, name, srcref)[[1]]
		res <- list()
		if( file.exists(file) ){
			res$bibliography <- normalizePath(file)
#				bibfiles <- roxygenGlobal('bibfiles')
#				# add the file to the global vector of bibliography files
#				if( length(bibfiles) == 0 || !file %in% names(bibfiles) ){
#					bibfiles <- c(bibfiles, FALSE)
#					names(bibfiles)[length(bibfiles)] <- file
#					roxygenGlobal('bibfiles', bibfiles)
#				}			
		}else{
			roxygen_warning("Bibliography file '", file, "' does not exists", srcref=srcref)
		}
		res
	}
)

# parser for @cite tags: prepare data for format.references
register.preref.parser('cite', function(key, name, srcref) {
		keys <- str_split(str_trim(name), "\\s+")[[1]]		
		# add srcref info
#			res <- setNames(str_c("@cite:", srcref$filename
#							, ':', srcref$lloc[1], '-', srcref$lloc[3],':'
#							, keys), rep('cite', length(keys)))
		res <- setNames(keys, rep('cite', length(keys)))
		as.list(res)
	}
)

#' @export
bibliography_roclet <- function() {
	new_roclet(list, "bibliography")
}

# creates an empty bibentry object suitable for concatenation
emptybib <- function(){ x <- list(); class(x) <- 'bibentry'; x}

# returns the path to the package standard REFERENCES.bib file
pkgBibfile <- function(path){
	file.path(path, 'inst/REFERENCES.bib')
}

#' @S3method roc_process bibliography
roc_process.bibliography <- function(roclet, partita, base_path) {
	# Remove srcrefs with no attached roxygen comments
	partita <- Filter(function(x) length(x) > 1, partita)	
	#message("Roclet bibliography")	
	
	# gather all bibliography files
	bibfiles <- NULL 
	for (partitum in partita) {		
		f <- unique(unlist(partitum[names(partitum) == "bibliography"]))
		bibfiles <- c(bibfiles, f)
	}
	# add standard REFERENCES.bib file on top
	std_bibfile <- pkgBibfile(base_path)
	if( file.exists(std_bibfile) )
		bibfiles <- c(normalizePath(std_bibfile), bibfiles)	
	#print(bibfiles)
	
	# lookup for the bibentries
	refs <- emptybib()
	need_reffile <- FALSE
	for (partitum in partita) {
		# get all citation keys
		bkeys <- unlist(partitum[names(partitum) == "cite"])
		bkeys <- unique(bkeys)
		# lookup keys in cached bibfiles
		if( !is.null(bkeys) ){
			need_reffile <- TRUE
			refs <- c(refs, lookupBibentry(bkeys, bibfiles, partitum))
		}
	}
	#str(refs)
	
	# warn about REFERENCES.bib
	if( !need_reffile && file.exists(std_bibfile) )
		message("NOTE - No @cite tags were detected: file 'inst/REFERENCES.bib' not used for Rd files [but maybe for vignettes].")		
	
	# return the references that need to be added
	refs
}

#' @S3method roc_output bibliography
roc_output.bibliography <- function(roclet, results, base_path) {
	
	# update standard bib file
	std_bibfile <- pkgBibfile(base_path)
	if( length(results) > 0L ){
		rd_out_cache$compute(results, {
			message("Updating file inst/REFERENCES.bib ... ", appendLF=FALSE)
			write.bib(results, file=std_bibfile, append=TRUE, verbose=FALSE)
			message("OK")
		})
	}
	
}

# select bibentries from a set of bibentries or a bib file 
getBibEntry <- function(key, bibentry){
	
	# load from file if necessary
	if( !is(bibentry, 'bibentry') ){
		if( !file.exists(bibentry) ) return(emptybib())
		bibentry <- bibtex::read.bib(bibentry)
	}
	k <- unlist(bibentry$key)
	bibentry[k %in% key]
	
}

# Lookup References from Bibliography Files
lookupBibentry <- function(keys, bibfiles, partitum){
	
	.loadBibfile <- function(bibfile){
		message("\nLoading bibliography file '", bibfile, "' ... ", appendLF=FALSE)
		info <- file.info(bibfile)
		if( info$size != 0 )
			suppressWarnings(suppressMessages(capture.output(bibs <- bibtex::read.bib(bibfile))))
		else bibs <- emptybib()
		bibs		
	}
	
	# lookup one key in bibfiles until finding it.
	# bibfiles are loaded and cached based on their md5 sum
	.lookup <- function(k, bibfiles){
		message("Lookup for citation key '", k, "' ... ", appendLF=FALSE)
		for(i in seq_along(bibfiles) ){
			f <- bibfiles[i]
			hash <- c(f, tools::md5sum(f))
			# load/compute bib databases in cache
			bib_db <- rd_proc_cache$compute(hash,.loadBibfile(f))
			bibitem <- getBibEntry(k, bib_db)
			if( length(bibitem) > 0L ){
				message("OK")
				# return result only if was not found in the first bibfile 
				# which is the package REFERENCES.bib file
				if( i > 1L ) return(bibitem) else return(NULL);
			}
		}
		message("NA")
		#roxygen_warning("Citation key '", k, "' not found", srcref=partitum$srcref)
		NULL
	}
	
	bibs <- emptybib()
	lapply(keys, function(k){
		hash <- c(k, bibfiles, tools::md5sum(bibfiles))
		bibitem <- rd_proc_cache$compute(hash, .lookup(k, bibfiles))
		# add bibitem to the result only if needed
		if( length(bibitem) > 0L ){
			bibs <<- c(bibs, bibitem)
		}
	})
	bibs
}