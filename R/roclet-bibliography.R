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

# parser for @cite tags: separate BibTeX keys
register.preref.parser('cite', function(key, name, srcref) {
		keys <- str_split(str_trim(name), "\\s+")[[1]]		
		res <- setNames(keys, rep('cite', length(keys)))
		as.list(res)
	}
)

#' Roclet to Generate File REFERENCES.bib
#' 
#' This roclet generates a file REFERENCES.bib in the package's \dQuote{inst} 
#' sub-directory, based on BibTeX database files declared with tag \code{@@bibliography} 
#' citations specified with tag \code{@@cite}.
#' 
#'@section Specific tags:
#'
#' This roclet processes the following tags:
#'
#' \describe{
#'
#'  \item{\code{@@bibliography filename}}{Declare a BibTeX database file where 
#' citation keys are looked for.
#' Multiple database files are declared using multiple \code{@@bibliography} tags, 
#' one for each file.
#' The files are searched by order of declaration, and the first entry that matches 
#' the looked-up key is used to generate a full reference string.
#' 
#' \code{@@bibliography} tags are typically put in the roxygen chunk that describes the package
#' (i.e.. that contains the tag \code{@@docType package}).  
#' 
#' There is no need to add a tag \code{@@bibliography inst/REFERENCES.bib}, as 
#' this BibTeX file is searched by default if already present. 
#' }
#'
#'  \item{\code{@@cite space separated BibTeX citation keys}}{Keys of BibTeX entries 
#' that will be substituted by full references and inserted in the \emph{References} 
#' section of the corresponding Rd file.
#' 
#' \code{@@cite} tags should be put in the roxygen chunks corresponding to the Rd 
#' where they must appear.
#' }
#'  
#'}
#'  
#' @author Renaud Gaujoux
#' @family roclets
#' @export
#' @examples
#' 
#' #' A very nice package
#' #'
#' #' @@name NicePkg
#' #' @@docType package
#' #' @@bibliography /home/username/articles/library.bib
#' NULL
#'
#' #' Some function
#' #'
#' #' This function implements the method from Toto (2010). 
#' #'
#' #' @@cite Toto2010
#' fun <- function() {}
#'
#' roclet <- bibliography_roclet()
#' \dontrun{roc_proc(roclet, "example.R")}
#' \dontrun{roc_out(roclet, "example.R", ".")}

#' 
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
	has_reffile <- file.exists(std_bibfile) 
	if( has_reffile )
		bibfiles <- c(normalizePath(std_bibfile), bibfiles)
	#str(bibfiles)
	
	# lookup for the bibentries
	refs <- emptybib()
	any_cite <- FALSE
	for (partitum in partita) {
		# get all citation keys
		bkeys <- unlist(partitum[names(partitum) == "cite"])
		bkeys <- unique(bkeys)
		# lookup keys in cached bibfiles
		if( !is.null(bkeys) ){
			any_cite <- TRUE
			refs <- c(refs, lookupBibentry(bkeys, bibfiles, partitum, has_reffile))
		}
	}
	#str(refs)
	
	# warn about REFERENCES.bib
	if( !any_cite && has_reffile )
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
			# create inst sub-directory if necessary
			if( !file.exists(dirname(std_bibfile)) ){
				dir.create(dirname(std_bibfile), recursive=TRUE)
			}
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
		bibentry <- BibTeX::read.bib(bibentry)
	}
	k <- unlist(bibentry$key)
	bibentry[k %in% key]
	
}

# Lookup References from Bibliography Files
lookupBibentry <- function(keys, bibfiles, partitum, skip=TRUE){
	
	.loadBibfile <- function(bibfile){
		message("\nLoading bibliography file '", bibfile, "' ... ", appendLF=FALSE)
		info <- file.info(bibfile)
		if( info$size != 0 )
			suppressWarnings(suppressMessages(capture.output(bibs <- BibTeX::read.bib(bibfile))))
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
				if( !skip || i > 1L ) return(bibitem) else return(NULL);
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

# Process tags @cites: load full reference from inst/REFERENCES.bib file
# that has been generated by the bibliography roclet.
process.cite <- function(partitum, base_path){
	process_had_tag(partitum, "cite", function(tag, param){		
		# load entry from the standard REFERENCES.bib:
		# It should have already been updated by roclet_bibliography as necessary
		bib <- getBibEntry(param, pkgBibfile(base_path))
		# substitute citation keys with the formated reference
		if( length(bib) > 0L ){
			param <- format(bib)
		}else
			roxygen_warning("Unresolved BibTeX key '", param, "'", srcref=partitum$srcref)
		new_tag("references", param)	  
	})
}
