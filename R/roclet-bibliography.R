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
		}else{
			roxygen_warning("Bibliography file '", file, "' does not exists", srcref=srcref)
		}
		res
	}
)

parse.cite <- function(key, name, srcref) {
	keys <- str_split(str_trim(name), "[, ]+")[[1]]		
	res <- setNames(keys, rep('cite', length(keys)))
	as.list(res)
}

# Extract \cite commands from a string and returns them separated in a named list
# with all names equal to 'cite'
extract.cite <- function(key, str, noescape=FALSE){
	
	if( key %in% c('examples') ) return()
	
#	if( grepl("rcite", str))
#		print(str)	
	# check for the presence of \cite commands
	p <- if( noescape ) "[^\\]?\\rcite\\{([^}]*)\\}" else "[^\\]\\\\rcite\\{([^}]*)\\}"  
	cite <- str_match_all(str, p)
	if( length(cite[[1]]) > 0L ){
		#print(cite)
		cite <- unlist(lapply(cite[[1]][,2], parse.cite, key='cite'))
		as.list(setNames(unlist(cite), rep('cite', length(cite))))
	}
}
# parser for @cite tags: separate BibTeX keys
register.preref.parser('cite', parse.cite)

#' Roclet to Generate File REFERENCES.bib
#' 
#' This roclet generates a file REFERENCES.bib in the package's \dQuote{inst} 
#' sub-directory, based on BibTeX database files declared with tag \code{@@bibliography} 
#' citations specified with tag \code{@@cite}.
#' 
#'@section Specific tags and command:
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
#'  \item{\code{@@cite space/comma separated BibTeX citation keys}}{Keys of BibTeX entries 
#' that will be substituted by full references and inserted in the \emph{References} 
#' section of the corresponding Rd file.
#' 
#' \code{@@cite} tags should be put in the roxygen chunks corresponding to the Rd 
#' where they must appear.
#' }
#' 
#' \item{\code{\\rcite{single BibTeX citation key}}}{This 
#' command can be used within the body of roxygen tags, e.g. in descriptions, 
#' details, sections, etc...
#' 
#' The command is substituted in the Rd file by a quick reference, wrapped in a 
#' \code{\\cite}, while a full reference is added in the \emph{References} section.
#' Unresolved keys are left unchanged, also wrapped in a \code{\\cite} command.
#' 
#' For example:
#' 
#' - \\rcite{Toto2008} would sds generate something like \dQuote{\cite{Toto et al. (2008)}}
#' 
#' - If unresolved, \\rcite{Toto2010} would generate \dQuote{\cite{Toto2010}}
#' 
#' }
#'  
#'}
#'  
#' @bibliography cite/inst/tests/REFdb.bib
#' @author Renaud Gaujoux
#' @family roclets
#' @export
#' @examples
#' 
#' #' An example file, example.R, which document a package
#' #'
#' #' A very nice package.
#' #' For more info see \rcite{Somebody2012}
#' #'
#' #' @@name NicePkg
#' #' @@docType package
#' #' @@bibliography /home/username/articles/library.bib
#' NULL
#'
#' #' Some function
#' #'
#' #' This function implements the method from \rcite{Toto2008}. 
#' #'
#' #' @@cite Tata1980
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
pkgBibfile <- local({
	.path <- NULL
	function(path){
		if( !missing(path) ) .path <<- path
		if( is.null(.path) )
			stop("Unexpected error: Bibliography file is not set.")
		
		file.path(.path, 'inst/REFERENCES.bib')
	}
})

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
getBibEntry <- function(key, bibentry=pkgBibfile()){
	
	# load from file if necessary
	if( !is(bibentry, 'bibentry') ){
		if( !file.exists(bibentry) ) return(emptybib())
		bibentry <- bibtex::read.bib(bibentry)
	}
	k <- unlist(bibentry$key)
	i <- match(key, k, nomatch=0L)
	bibentry[i[i!=0L]]
}

# Lookup References from Bibliography Files
lookupBibentry <- function(keys, bibfiles, partitum, skip=TRUE){
	
	# initialize result
	bibs <- emptybib()
	
	# exit early if no bibfiles were passed
	if( length(bibfiles) == 0L ) return(bibs)
	
	.loadBibfile <- function(bibfile){
		message("Loading bibliography file '", bibfile, "' ... ", appendLF=FALSE)
		info <- file.info(bibfile)
		if( info$size != 0 )
			suppressWarnings(suppressMessages(capture.output(bibs <- bibtex::read.bib(bibfile))))
		else bibs <- emptybib()
		message("OK")
		bibs		
	}
	
	# lookup one key in bibfiles until finding it.
	# bibfiles are loaded and cached based on their md5 sum
	.lookup <- function(k, bibfiles){
		#message("Lookup for citation key '", k, "' ... ", appendLF=FALSE)
		for(i in seq_along(bibfiles) ){
			f <- bibfiles[i]
			hash <- c(f, tools::md5sum(f))
			# load/compute bib databases in cache
			bib_db <- rd_proc_cache$compute(hash,.loadBibfile(f))
			bibitem <- getBibEntry(k, bib_db)
			if( length(bibitem) > 0L ){
				#message("OK")
				# return result only if was not found in the first bibfile 
				# which is the package REFERENCES.bib file
				if( !skip || i > 1L ) return(bibitem) else return(NULL);
			}
		}
		#message("NA")
		#roxygen_warning("Citation key '", k, "' not found", srcref=partitum$srcref)
		NULL
	}
	
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
	
	keys <- unlist(unique(partitum[names(partitum) == 'cite']))
	if (length(keys) == 0) return()
	
	# load entry from the standard REFERENCES.bib:
	# It should have already been updated by roclet_bibliography as necessary
	bib <- getBibEntry(keys, pkgBibfile(base_path))
	# substitute citation keys with the formated reference
	ik <- keys %in% bib$key
	if( length(bib) > 0L ){
		keys[ik] <- format(bib)
	}	
	if( length(bib) != length(keys) )
		roxygen_warning("Unresolved BibTeX key(s) ", str_c("'", keys[!ik], "'", collapse=','), srcref=partitum$srcref)
	
	# return tags with formated references
	unlist(lapply(keys, function(p) new_tag("references", p)), recursive = FALSE)	
}

shortcite <- function(b){
	
	res <- b$author[1]$family
	if( length(b$author) > 1L )
		res <- str_c(res, " et al.")
	str_c(res, " (", b$year, ')')
}

format_cite <- function(str){
	cite <- extract.cite('cite', str)
	if( length(cite) > 0L ){
		for(ci in cite){
			b <- getBibEntry(ci)
			e <- if( length(b) == 0L ) ci else shortcite(b)	
			str <- gsub(str_c("([^\\])\\\\rcite\\{", ci, "\\}"), str_c("\\1\\\\cite{", e, "}"), str)
		}
	}
	str
}
