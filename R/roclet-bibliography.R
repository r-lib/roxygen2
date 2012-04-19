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
extract.cite <- function(key, str, full=FALSE){
	
	# do not extract from some specific tags
	if( key %in% c('examples') ) return()
	
#	if( grepl("cite", str))
#		print(str)	
	# check for the presence of \cite commands
	p <- "(^|[^\\])(\\\\cite\\{([^} ]*)\\})"  
	cite <- str_match_all(str, p)
	#print(cite)
	if( length(cite[[1]]) > 0L ){
		#print(cite)
		if( !full ){
			cite <- unlist(lapply(cite[[1]][,4], parse.cite, key='cite'))
			as.list(setNames(unlist(cite), rep('cite', length(cite))))
		}else{
			mapply(function(s, cmd){
				list(keys=unlist(parse.cite(s, key='cite'))
					, cmd=cmd, skeys=s)
			}, cite[[1]][,4], cite[[1]][,3], SIMPLIFY=FALSE)
		}
	}
}
# parser for @cite tags: separate BibTeX keys
register.preref.parser('cite', parse.cite)

#' Roclet to Generate File REFERENCES.bib
#' 
#' This roclet generates a file REFERENCES.bib in the package's \dQuote{inst} 
#' sub-directory, based on BibTeX database files declared with tags \code{@@bibliography} 
#' and citations specified with tags \code{@@cite}.
#' 
#'@section Specific tags and command:
#'
#' This roclet processes the following tags:
#'
#' \describe{
#'
#'  \item{\code{@@bibliography filename}: }{Declare a BibTeX database file where 
#' citation keys from \code{@@cite} tags and \code{\\cite} commands are looked for.
#' Multiple database files are declared using multiple \code{@@bibliography} tags, 
#' one for each file.
#' The files are searched by order of declaration, and the first entry that matches 
#' the looked-up key is used to generate a full reference string in the Rd files' 
#' \emph{References} sections.
#' 
#' \code{@@bibliography} tags are typically put in the roxygen chunk that describes 
#' the package being documented (i.e. the chunk that contains the tag 
#' \code{@@docType package}).
#' However they can be put in any other chunk, in any given collated R file.  
#' 
#' There is no need to manually create the package's default BibTeX file 
#' (\dQuote{inst/REFERENCES.bib}) or add a \code{@@bibliography} tag to declare it,
#' as it is always searched if present -- and is created automatically if any 
#' citation was found in roxygen chunks and an external bibliography file is declared. 
#' }
#'
#'  \item{\code{@@cite space/comma separated BibTeX citation keys}: }{Keys of BibTeX entries 
#' that will be substituted by full references and inserted in the \emph{References} 
#' section of the corresponding Rd file.
#' 
#' \code{@@cite} tags should be put in the roxygen chunks corresponding to the Rd 
#' where they must appear.
#' }
#' 
#' \item{\code{\\cite{comma separated BibTeX citation keys}}: }{This 
#' command can be used within the body of any relevant roxygen tags, e.g. 
#' in @@descriptions, @@details, @@sections, etc...
#' 
#' The commands that contain BibTeX citation keys are substituted in the Rd file 
#' by short citation strings, still wrapped in a \code{\\cite} command that is 
#' rendered accordingly by R Rd engine (e.g. \cite{Toto et al. (2008)}), 
#' and a full reference is added in the \emph{References} section.
#' Unresolved citation keys are left unchanged, also wrapped in a 
#' \code{\\cite} command.
#' 
#' The \code{\\cite} commands are substituted in the generated Rd files 
#' with the following rules:
#' 
#' \itemize{
#' 
#' \item If the citation key \dQuote{Toto2008} exists in one of the declared 
#' BibTeX files, then the command \code{\\cite{Toto2008}} is substituted by 
#' \code{\\cite{Toto (2008)}} or \code{\\cite{Toto et al. (2008)}} if the 
#' reference is from a single or multiple authors respectively;
#' 
#' \item Multiple citations are specified with commas (no space allowed, see last rule)
#' , e.g. \code{\\cite{Toto2008,Tata1989}}, and are substituted as: 
#' \dQuote{\cite{Toto et al. (2008), Tata (1989)}};
#' 
#' \item Unresolved citation keys (e.g. Unknown2010) remain unchanged: 
#' \code{\\cite{Unknown2010}} remains \code{\\cite{Unknown2010}} and 
#' \code{\\cite{Tata1989,Unknown2010}} becomes \code{\\cite{Tata (1989), Unknown2010}};
#' 
#' \item Escaped commands \code{\\\\cite} are not substituted;
#' 
#' \item Commands that contain spaces are not considered as being BibTeX 
#' citations, and are therefore ignored by roxygen.
#' Hence roxygen will not try to find a BibTeX entry from the command 
#' \code{\\cite{Writing R Extensions}}, which will remain unchanged.
#' In the -- unlikely -- case where one wants to use \code{\\cite} with a 
#' single word, that also happens to be a BibTeX citation key, then a 
#' leading or trailing space should be added -- or the BibTeX entry key be changed.  
#' 
#' }% end itemize
#' 
#' }
#'  
#'}
#'  
#' @author Renaud Gaujoux
#' @family roclets
#' @export
#' @examples
#' 
#' ### An example file, example.R, which document a package
#' #'
#' #' A very nice package.
#' #' For more info see \cite{Somebody2012}
#' #'
#' #' @@name NicePkg
#' #' @@docType package
#' #' @@bibliography /home/username/articles/library.bib
#' NULL
#'
#' #' Some function
#' #'
#' #' This function implements the method from \cite{Toto2008}. 
#' #'
#' #' @@cite Tata1980
#' fun <- function() {}
#' ###
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

# create short citation from a single bibentry
shortcite <- function(b){
	
	res <- b$author[1]$family
	if( length(b$author) > 1L )
		res <- str_c(res, " et al.")
	str_c(res, " (", b$year, ')')
}

# substitutes \cite commands with short citations in a string, based on a BibTeX
# database file possibly passed in ...:
#
# \cite{Toto2010} -> \cite{Toto (2010)} or \cite{Toto et al. (2010)}
# \cite{Toto2010, Tata1989} -> \cite{Toto et al. (2010), Tata (1989)}
format_cite <- function(str, ...){
	cite <- extract.cite('cite', str, full=TRUE)
	if( length(cite) > 0L ){		
		for(ci in cite){
			#message("#cmd=", ci$cmd)
			#message("#str in=", str)
			e <- sapply(ci$keys, function(x){
				b <- getBibEntry(x, ...)
				if( length(b) == 0L ) x else shortcite(b)
			})
			e <- str_c(e, collapse=", ")
			ncmd <- gsub(ci$skeys, e, ci$cmd, fixed=TRUE)
			ncmd <- gsub("\\", "\\\\", ncmd, fixed=TRUE)
			#print(ncmd)
			pa <- str_replace_all(ci$cmd, "([\\{}])", "\\\\\\1")
			#print(pa)
			str <- gsub(str_c("(^|[^\\])", pa), str_c("\\1", ncmd), str)
			#message("#str out=", str)
		}
	}
	str
}
