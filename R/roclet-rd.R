#' @include parse-registry.R
#' @import stringr
NULL

register.preref.parsers(parse.value,
                        'name', 
                        'rdname',
                        'aliases',
                        'title',
                        'usage',
                        'references',
                        'concept',
                        'note',
                        'seealso',
                        'example',
                        'examples',
                        'keywords',
                        'return',
                        'author',
                        'section',
                        'family',
                        'inheritParams',
                        'format',
                        'source', 
                        'encoding',
                        'description',
                        'details')

register.preref.parsers(parse.name.description,
                        'param',
                        'slot',
                        'method')

register.preref.parsers(parse.name,
                        'docType')

register.preref.parsers(parse.default,
                        'noRd')

register.preref.parsers(parse.toggle,
						'inline')

#' Roclet: make Rd files.
#'
#' This roclet is the workhorse of \pkg{roxygen}, producing the Rd files that
#' document that functions in your package.  
#'
#' This roclet also automatically runs \code{\link[tools]{checkRd}} on all
#' generated Rd files so that you know as early as possible if there's a
#' problem.
#'
#' @section Required tags:
#' 
#' As well as a title and description, extracted from the first sentence and
#' first paragraph respectively, all functions must have the following tags:
#'
#' \describe{
#'
#'  \item{\code{@@param name description}}{Document a parameter.  
#'    Documentation is required for every parameter.}
#'
#'  \item{\code{@@inheritParams source_function}}{Alternatively, you can
#'    inherit parameter description from another function. This tag will
#'    bring in all documentation for parameters that are undocumented in the
#'    current function, but documented in the source function. The source 
#'    can be a function in the current package, \code{function}, or another
#'    package \code{package::function}.}
#'
#'  \item{\code{@@method generic class}}{Required if your function is an S3
#'    method.  This helps R to distinguish between (e.g.) \code{t.test} and 
#'    the \code{t} method for the \code{test} class.}
#'
#'  }
#'
#' @section Optional tags that add extra information:
#'
#' Valid tags for \code{rd_roclet} are:
#' 
#' \describe{
#'
#'  \item{\code{@@examples R code...}}{Highly recommended: example code that
#'    demonstrates how to use your function.  Use \\dontrun{} to tag code
#'    that should not automatically be run.}
#' 
#'  \item{\code{@@example path/relative/to/packge/root}}{Instead of including
#'    examples directly in the documentation, you can include them as 
#'    separate files, and use the \code{@@example} tag to insert them into
#'    the documentation.}
#'
#'  \item{\code{@@return}}{Used to document the object returned by the 
#'    function. For lists, use the \code{\\item{name a}{description a}} 
#'    describe each component of the list}
#'
#'  \item{\code{@@author authors...}}{A free text string describing the 
#'    authors of the function.  This is typically only necessary if the
#'    author is not the same as the package author.}
#'
#'  \item{\code{@@note contents}}{Create a note section containing additional
#'    information.}
#' 
#'  \item{\code{@@section Name: contents}}{Use to add to an arbitrary section
#'    to the documentation. The name of the section will be the content before
#'    the first colon, and the contents will be everything after the colon.} 
#'
#'  \item{\code{@@keywords keyword1 keyword2 ...}}{Keywords are optional, 
#'    but if present, must be taken from the list in 
#'    \file{file.path(R.home(), "doc/KEYWORDS")}.  Use the \code{internal}
#'    keyword for functions that should not appear in the main function
#'    listing.}
#'
#'  }
#'
#' @section Optional tags for cross-referencing:
#'
#' \describe{
#'
#'  \item{\code{@@aliases space separated aliases}}{Add additional aliases, 
#'    through which the user can find the documentation with \code{?}. 
#'    The topic name is always included in the list of aliases.}
#' 
#'  \item{\code{@@concepts space separated concepts}}{Similar to
#'    \code{@@aliases} but for \code{\link{help.search}}}
#'
#'  \item{\code{@@references free text reference}}{Pointers to the literature
#'    related to this object.}
#'
#'  \item{\code{@@seealso Text with \\code{\\link{function}}}}{Pointers to
#'    related R objects, and why you might be interested in them.}
#'
#'  \item{\code{@@family family name}}{Automatically adds see-also
#'     cross-references between all functions in a family. A function can
#'     belong to multiple families.}
#'  }
#'
#' @template template
#' @section Optional tags that override defaults:
#'
#' These tags all override the default values that roxygen guess from 
#' inspecting the source code.
#' 
#' \describe{
#'
#'  \item{\code{@@rdname filename}}{Overrides the output file name (without
#'    extension). This is useful if your function has a name that is not
#'    a valid filename (e.g. \code{[[<-}), or you want to merge documentation
#'    for multiple function into a single file.}
#' 
#'  \item{\code{@@title Topic title}}{Specify the topic title, which by 
#'    by default is taken from the first sentence of the roxygen block.}
#'
#'  \item{\code{@@usage usage_string}}{Override the default usage string. 
#'    You should not need to use this tag - if you are trying to document
#'    multiple functions in the same topic, use \code{@@rdname}.}
#'
#'  }
#'
#' @section Tags for non-functions:
#'
#' These tags are useful when documenting things that aren't functions, 
#' datasets and packages.
#'
#' \describe{
#'
#'  \item{\code{@@name topicname}}{Override the default topic name, which is
#'    taken by default from the object that is assigned to in the code
#'    immediately following the roxygen block. This tag is useful when
#'    documenting datasets, and other non-function elements.}
#'
#'  \item{\code{@@docType type}}{Type of object being documented. Useful 
#'    values are \code{data} and \code{package}. Package doc type will
#'    automatically add a \code{package-} alias if needed.}
#' 
#'  \item{\code{@@format description}}{A textual description of the format
#'    of the object.}
#'
#'  \item{\code{@@source text}}{The original source of the data.}
#' 
#'}
#' @family roclets
#' @examples
#' roclet <- rd_roclet()
#' \dontrun{roc_proc(roclet, "example.R")}
#' \dontrun{roc_out(roclet, "example.R", ".")}
#' @export
rd_roclet <- function() {
  new_roclet(list(), "had")
}

#' @S3method roc_process had
#' @importFrom digest digest
roc_process.had <- function(roclet, partita, base_path) {
  # Remove srcrefs with no attached roxygen comments
  partita <- Filter(function(x) length(x) > 1, partita)
  templates <- dir(file.path(base_path, "max-roxygen"), full = TRUE)
  template_hash <- digest(lapply(templates, readLines))
  
  topics <- list()
  # reset rd_lookup (use not really as a cache, but rather as a global environment)
  rd_lookup_cache$reset()
  for (partitum in partita) {
	
	## Dealing with tags that have an impact across rd file
	## must be done out of the cached processing
	# @inline: @inline on a generic enforces it on its methods
	partitum <- process.inline(partitum)
	##
	  
    key <- c(template_hash, digest(partitum))
    new <- rd_proc_cache$compute(key, roclet_rd_one(partitum, base_path)) 
    if (is.null(new)) next; 

	# add rd to lookup: access key is default topic name
	# (subset with [1L] to get data naked from attributes)
	rd_lookup_cache$compute(new$rdID[1L], list(hash=key, filename=new$filename))

    # Clone output so cached object isn't modified
    new$rd[[1]] <- list2env(as.list(new$rd[[1]]))
	
	# add internal tag S4method to the topics of the  
	# parent S4 generic and the S4 class of the first dispatched argument. 
	# Inline S4 methods are merged into the parent generic topic.
	# NB: changes occur on rd_file objects from the list `topics` not from the cache
	addS4method(topics, new, partitum)
    
	# merge topic
    old <- topics[[new$filename]]
    topics[[new$filename]] <- if (is.null(old)) new$rd else merge(old, new$rd)
  }
  
  # Second parse through to process @family
  invert <- function(x) {
    if (length(x) == 0) return()
    unstack(rev(stack(x)))
  }
  get_values <- function(topics, tag) {
    tags <- lapply(topics, get_tag, tag)
    tags <- Filter(Negate(is.null), tags)
    lapply(tags, "[[", "values")
  }
  
  family_lookup <- invert(get_values(topics, "family"))
  name_lookup <- get_values(topics, "rdID")

  for(family in names(family_lookup)) {
	# get rd_file objects that should include links
	# use unique to remove duplications due to rd_file merging
    related <- unique(family_lookup[[family]])
    
    for(topic_name in related) {
      topic <- topics[[topic_name]]
      others <- setdiff(related, topic_name)
      
      if (length(others) < 1) next;
      
      other_topics <- sort(unlist(name_lookup[others], use.names = FALSE))

      links <- paste("\\code{\\link{", other_topics, "}}", 
        collapse =", ", sep = "")
      seealso <- paste("Other ", family, ": ", links, sep = "")
      
      add_tag(topic, new_tag("seealso", seealso))
    }
  }
  
  # And to process @inheritParams
  
  # Currently no topological sort, so @inheritParams will only traverse
  # one-level - you can't inherit params that have been inherited from
  # another function (and you can't currently use multiple inherit tags)
  inherits <- get_values(topics, "inheritParams")
  # get srcref data to build detailed and informative warnings
  srcref_lookup <- get_values(topics, "srcref")
  
  # define warning function that gives details on the roxygen chunk
  inherit_warning <- function(topic, subtopic){
	  function(..., srcref=NULL){
	  
		  # default is to show srcref details of child-topic
		  child_srcref <- srcref_lookup[[topic]][subtopic]
		  if( is.null(srcref) ){
			  srcref <- child_srcref
			  header <- "@inheritParams - "
		  }else{
			  header <- str_c("@inheritParams (", srcref_location(child_srcref), ") - ")
		  }
		  # throw warning
		  roxygen_warning(header, ..., srcref = srcref, immediate. = TRUE)
		  
	  }	  
  }
  
  
  list_missing_params <- function(topic){
	  setdiff(get_tag(topic, "formals")$values,
			  names(get_tag(topic, "arguments")$values))
  }
  
  for(topic_name in names(inherits)) {
	#message("# Topic: ", topic_name)
    topic <- topics[[topic_name]]
	
	# 1. check if it is necessary to pull @param descriptions from parent topics 
	missing_params <- list_missing_params(topic)
	if( length(missing_params) == 0L ) next;
	#message("Missing arguments: ", toString(missing_params))
	
	# 2. lookup for the missing arguments
    for(i in seq_along(inherits[[topic_name]]) ){
	  # get inheritParam target
	  inheritor <- inherits[[topic_name]][i]
	  # setup warning function
	  warn <- inherit_warning(topic_name, names(inherits[[topic_name]])[i])
	  
	  #message("# Looking up topic: ", inheritor)
      if (grepl("::", inheritor, fixed = TRUE)) {
        # Reference to another package
        pieces <- strsplit(inheritor, "::", fixed = TRUE)[[1]]
		params <- get_rd(pieces[2], pieces[1])
		if (length(params) == 0L){
			warn("can't find parent topic `", pieces[2], "` from package ", pieces[1])
		}else{
			params <- rd_arguments(params)
			if (length(params) == 0L){
				warn("can't find argument section for parent topic `", pieces[2], "` from package ", pieces[1])
			}
		}
      } else {
		#message("# Look within package")
        # Reference within this package        
        rd_name <- names(Filter(function(x) inheritor %in% x, name_lookup))
        params <- 
        if (length(rd_name) == 0L){
			warn("can't find parent topic `", inheritor, "`")
			list()
		}else{
			#message("# Found in Rd file: ", if( length(rd_name) > 0L ) str_c("'", rd_name,"'", collapse=", "))
			if( length(rd_name) > 1L ){

				# show srcref of origin of name duplication
				srcref <- srcref_lookup[names(srcref_lookup) %in% rd_name]
				srcref <- lapply(srcref, function(x) x[[inheritor]])
				warn("multiple matches for parent topic `"
							, inheritor, "`: ", srcref=srcref)
				list()
			}else{
				# extract arguments from the computed Rd file structure
				get_tag(topics[[rd_name]], "arguments")$values  
			}
		}
      }
      params <- unlist(params)

	  # skip if topic documentation cannot not be improved
	  if( length(params)  == 0L ) next
	  
      matching_params <- intersect(missing_params, names(params))
	  #if( length(matching_params) ) message("Found arguments: ", toString(matching_params))
      add_tag(topic, new_tag("arguments", params[matching_params], inheritor))
	  
	  # update missing_params
  	  missing_params <- list_missing_params(topic)
	  if( length(missing_params) == 0L ) break;

    }

	# check for unfound arguments
	missing_params <- which(! get_tag(topic, "formals")$values 
							%in% names(get_tag(topic, "arguments")$values))
	
	# throw description warning on unfound arguments but skip inline topics 
	# since their arguments are handled in the parent topic and 
	# they do not generate Rd files 
	if( length(missing_params) && !is_inline(topic) ){
		# get missing argument names
		params <- get_tag(topic, "formals")$values[missing_params]
		# get original rdID for these arguments
		ids <- get_tag(topic, "formals")$rdID[missing_params]
		params <- split(params, factor(ids, levels=unique(ids)))
		srcref <- mapply(function(id, p){
			# get the original processed partitum from cache
			rd_proc <- get_rd_proc(id)
			desc <- str_c(str_c("`", p, "`", collapse=', '), ' of ', id
						, '(', srcref_location(rd_proc$srcref, header=FALSE), ')')
			roxygen_warning("No inherited description for argument(s) ", desc
							, srcref=srcref_lookup[[topic_name]][1])
		}, unique(ids), params)
	
	}

  }
  
  
  topics
}

roclet_rd_one <- function(partitum, base_path) {  
  # Add in templates
  partitum <- process_templates(partitum, base_path)
  
  has_rd <- any(names(partitum) %in% c("description", "param", "return",
    "title", "example", "examples", "name", "rdname", "usage",
    "details", "introduction"))
  dont_rd <- any(names(partitum) == "noRd")
  if (!has_rd || dont_rd) return()
  
  # Define topic unique identifier
  rdID <- partitum$src_topic %||% partitum$src_name
  # Figure out topic name
  name <- partitum$name %||% rdID
  if (is.null(name)) roxygen_stop("Missing name", srcref = partitum$srcref)

  # Work out file name and initialise Rd object
  filename <- str_c(partitum$rdname %||% nice_name(name), ".Rd")
  rd <- new_rd_file()  

  add_tag(rd, new_tag("rdID", rdID))
  add_tag(rd, new_tag("encoding", partitum$encoding))
  add_tag(rd, new_tag("inline", partitum$inline))
  add_tag(rd, new_tag("name", name))
  add_tag(rd, new_tag("alias", partitum$name %||% partitum$src_alias))
  add_tag(rd, new_tag("formals", names(partitum$formals), rep(rdID, length(partitum$formals))))
  add_tag(rd, new_tag("srcref", setNames(list(partitum$srcref), rdID)))

  add_tag(rd, process_description(partitum, base_path))

  add_tag(rd, process_had_tag(partitum, 'aliases', function(tag, param) {
    new_tag('alias', str_split(str_trim(param), "\\s+")[[1]])
  }))
  add_tag(rd, process.usage(partitum))
  add_tag(rd, process.arguments(partitum))
  add_tag(rd, process.slot(partitum))
  add_tag(rd, process.docType(partitum))
  add_tag(rd, process_had_tag(partitum, 'note'))
  add_tag(rd, process_had_tag(partitum, 'family'))
  add_tag(rd, process_had_tag(partitum, 'inheritParams', function(tag, param){ 
	    new_tag(tag, setNames(param, rdID))
    }))
  add_tag(rd, process_had_tag(partitum, 'author'))
  add_tag(rd, process_had_tag(partitum, 'format'))
  add_tag(rd, process_had_tag(partitum, 'source'))
  add_tag(rd, process_had_tag(partitum, 'seealso'))
  add_tag(rd, process_had_tag(partitum, "references"))
  add_tag(rd, process_had_tag(partitum, 'concept'))
  add_tag(rd, process_had_tag(partitum, 'return', function(tag, param) {
      new_tag("value", param)
    }))
  add_tag(rd, process_had_tag(partitum, 'keywords', function(tag, param, all, rd) {
      new_tag("keyword", str_split(str_trim(param), "\\s+")[[1]])
    }))
  add_tag(rd, process_had_tag(partitum, 'section', process.section))
  add_tag(rd, process.examples(partitum, base_path))

  list(rd = rd, filename = filename
  		, rdID=rdID, srcref=partitum$srcref)
}

#' @S3method roc_output had
#' @importFrom tools checkRd
roc_output.had <- function(roclet, results, base_path) { 
  man <- normalizePath(file.path(base_path, "man"))
  
  contents <- vapply(results, FUN.VALUE = character(1), function(x) {
    rd_out_cache$compute(x, {
			# do not format for inlined topics: writing file will skip
			if( is_inline(x) ){
				rd_proc <- get_rd_proc(get_tag(x, 'rdID')$values[1])
				cat("Skipping inline doc:", rd_proc$filename, "\n")
				as.character(NA) 
			}else format(x)
		})
  })
  
  write_out <- function(filename, contents) {
	
	# skip NA contents (i.e. to inline docs)
	if( is.na(contents) ) return()
    if (the_same(filename, contents)) return()
    
    name <- basename(filename)
    if (!str_detect(name, "^[a-zA-Z][a-zA-Z0-9_.-]*$")) {
      cat("Skipping invalid filename: ", name, "\n")
    } else {
      cat(sprintf('Writing %s\n', name))
      writeLines(contents, filename)    
      try(checkRd(filename))
    }
    
  }
  the_same <- function(path, new) {
    if (!file.exists(path)) return(FALSE)

    old <- str_c(readLines(path), collapse = "\n")
    return(identical(old, new))
  }
  
  paths <- file.path(man, names(results))
  mapply(write_out, paths, contents)    
}

# Prefer explicit \code{@@usage} to a \code{@@formals} list.
process.usage <- function(partitum) {
  if (!is.null(partitum$usage)) {
    return(new_tag("usage", partitum$usage))
  }

  # Only function usages are generated here
  type <- partitum$docType %||% partitum$src_type
  if (!identical(type, "function") && !identical(type, "method")) {
    return(new_tag("usage", NULL))
  }

  if (type == "method") {
    signature <- str_c(partitum$signature, collapse = ",")
    fun_name <- str_c("\\S4method{", partitum$generic, "}{", signature, "}")
  } else {
    if (is.null(partitum$method)) {
      fun_name <- partitum$src_name
    } else {
      fun_name <- rd_tag('method', partitum$method[[1]], partitum$method[[2]])
    }
  }
  
  args <- usage(partitum$formals)
  if (str_detect(fun_name, fixed("<-"))) {
    fun_name <- str_replace(fun_name, fixed("<-"), "")
	# remove argument 'value' from the argument list:
	# correct usage specification is "fun(x, y) <- value"
	args <- str_replace(args, ", value$", "")
    new_tag("usage", str_c(fun_name, "(", args, ") <- value"))
  } else {
    new_tag("usage", str_c(fun_name, "(", args, ")"))
  }
}


#' Retrieve Processed rd_file Objects
#' 
#' @param id a rdID (i.e. a character string)
#' @param topics optional list of topics, indexed by Rd filename, from where to 
#' retrieve the rd_file object
#' @return a list as returned by \code{roclet_rd_one} that contains
#' the processed rd_file object if \code{topics=NULL} or a rd_file object 
#' otherwise. Returns \code{NULL} if the id is not found, meaning that the 
#' topic is not documented in the package or has not been processed yet.
#' 
#' @keyword internal
get_rd_proc <- function(id, topics=NULL){
	info <- rd_lookup_cache$get(id)
	if( is.null(info) ) return()
	# look into the cache 
	res <- rd_proc_cache$get(info$hash)
	if( !is.null(topics) )
		res$rd <- topics[[info$filename]]
	res
}

# get cached parent  
get_rd_proc_parent <- function(id, ...){
	
	if( is.list(id) ){ # id is assumed to be partitum
		partitum <- id
		# do something only for S4 method
		type <- partitum$src_type
		if( is.null(type) || type != 'method' ) return()
		# lookup generic name
		id <- partitum$generic
		if( is.null(id) ){
			roxygen_warning("Unexpectedly missing internal element $generic for S4 method"
					, srcref=partitum$srcref)
			return()
		}
	}
	
	# remove package part
	# (subset with [1L] to get data naked from attributes)
	parent <- sub("^.*::", "", id)[1L]
	get_rd_proc(parent, ...)	
}

is_inline <- function(x){
	
	if( is.rd_file(x) ){
		inline_tag <- get_tag(x, 'inline')$values
		rdID <- get_tag(x, 'rdID')$values[1]
	}else if( is.list(x) ){ # rd_file as a list of tags
		inline_tag <- x$inline$values
		rdID <- x$rdID$values[1]
	}else if( is.character(x) ){
		return( is_inline(get_rd_proc(id)$rd) )
	}
	else 
		stop("Invalid argument x [", class(x), ']')
	
	any( inline_tag %||% FALSE ) && grepl('-method$', rdID)
	
}

# enforces inline on S4 methods from generics that have tag @inline
process.inline <- function(partitum){
	# do something only for S4 method
	type <- partitum$src_type
	if( is.null(type) || type != 'method' ) return(partitum)
	
	# if @inline is on already (manually or determined in srcrefs.R)
	# then ensure tag is set and return
	if( partitum$inline %||% partitum$src_inline  %||% FALSE ){
		partitum$inline <- TRUE
		return(partitum)
	}
	
	## check if original parent generic is tagged with @inline:
	# a parent generic inline means its methods are inline
	parent_rd <- get_rd_proc_parent(partitum)
	# cannot have inline the parent topic is not documented in the package
	if( is.null(parent_rd) ) return(partitum)
	if( any(get_tag(parent_rd$rd, 'inline')$values) ){
		partitum$inline <- TRUE
		partitum$parent_inline <- TRUE 
	}
	
	# return updated partitum
	partitum
}

#' Automatic S4 Method Inline Documentation
#'
#' Adds a tag "S4method" to the Rd file that documents the generic for S4 methods 
#' that have no extra argument compared to their generic, or have been manually 
#' flagged with tag \emph{@@merge}.
#' This function works with a side-effect on both the current topic stored in 
#' \code{rd_proc} and the element of \code{topics} that stores the parent 
#' generic's topic.
#' 
#' @param topics list of topics already processed.
#' @param rd_proc result from the processing of argument \code{partitum} 
#' (as returned by \code{roclet_rd_one}).
#' @param partitum partitum that has just been processed.
#' 
#' @return Returns nothing, but changes its arguments \code{topics} and 
#' \code{rd__proc}
#'  
#' @keyword internal
addS4method <- function(topics, rd_proc, partitum){
	
	# do something only for S4 method
	type <- partitum$src_type
	if( is.null(type) || type != 'method' ) return()
	
	# Full inline documentation if @inline is on 
	inline_doc <- (partitum$inline %||% FALSE)
		
	# get parent in the list topics to add the S4method tag
	#, and merge if the method is inline
	parent_rd_proc <- get_rd_proc_parent(partitum, topics)
	# do nothing if the parent topic is not documented in the package
	# TODO: also tags to the class of first dispatching argument 
	if( is.null(parent_rd_proc) ) return()
	parent_rd <- parent_rd_proc$rd
	parent_id <- parent_rd_proc$rdID
	
	# build and add tag S4method to parent
	tags <- as.list(rd_proc$rd[[1]])
	mget_values <- function(x, tags){
		x <- x[ names(x) %in% tags ]
		lapply(x, "[[", "values")
	}
	# add details if inline
	s4tags <- c('description', 'title', if( inline_doc ) 'details')
	data <- list(introduction = mget_values(tags, s4tags) 
				, signature = partitum$signature)

	# skip description if it is identical to title
	if( identical(data$introduction$title, data$introduction$description) )
		data$introduction$description <- NULL

	# link to specific topic if not inline
	if( !inline_doc ){
		data$introduction$links <-  
			str_c("See \\code{\\link{", rd_proc$rdID, "}} for more details.")
	}
	add_tag(parent_rd, new_tag("S4method", setNames(list(data), parent_id)))
	
	# inline doc: merge it into parent 
	if( inline_doc ){
#		message("Merging ", rd_proc$rdID, " into ", parent_id
#				, " [inline:", if( partitum$src_inline ) 'auto' 
#				else if( partitum$parent_inline %||% FALSE ) 'parent'
#				else 'tag', ']')
		# merge into parent: all but introduction, keywords and alias tags
		# usage tag is skipped if the method was labelled as automatically merged
		skip_tags <- c(s4tags, 'inline', 'rdID', if( partitum$src_inline ) 'usage')
		tags <- tags[ !names(tags) %in% skip_tags ]
		add_tag(parent_rd, tags)
	}
	
}

# Process title, description and details. 
#
# Split the introductory matter into its description followed
# by details (separated by a blank line).
process_description <- function(partitum, base_path) {
  intro <- partitum$introduction
  
  if (!is.null(intro)) {
    paragraphs <- str_trim(str_split(intro, fixed('\n\n'))[[1]])
  } else {
    paragraphs <- NULL
  } 

  # 1st paragraph = title (unless has @title)
  if (!is.null(partitum$title)) {
    title <- partitum$title
  } else if (length(paragraphs) > 0) {
    title <- paragraphs[1]
    paragraphs <- paragraphs[-1]
  } else {
    title <- NULL
  }
  
  
  # 2nd paragraph = description (unless has @description)
  if (!is.null(partitum$description)) {
    description <- partitum$description
  } else if (length(paragraphs) > 0) {
    description <- paragraphs[1]
    paragraphs <- paragraphs[-1]
  } else {
    # Description is required, so if missing description, repeat title.
    description <- title
  }

  # Every thing else = details, combined with @details.
  details <- c(paragraphs, partitum$details)
  if (length(details) > 0) {
    details <- paste(details, collapse = "\n\n")
  } else {
    details <- NULL
  }

  c(new_tag("title", title),
    new_tag("description", description), 
    new_tag("details", details))
}

process.arguments <- function(partitum) {
  params <- partitum[names(partitum) == "param"]
  if (length(params) == 0) return() 

  desc <- str_trim(sapply(params, "[[", "description"))
  names(desc) <- sapply(params, "[[", "name")
  
  new_tag("arguments", desc)
}

process.slot <- function(partitum) {
  params <- partitum[names(partitum) == "slot"]
  if (length(params) == 0) return() 

  desc <- str_trim(sapply(params, "[[", "description"))
  names(desc) <- sapply(params, "[[", "name")
  
  new_tag("slot", desc)
}


# If \code{@@examples} is provided, use that; otherwise, concatenate
# the files pointed to by each \code{@@example}.
process.examples <- function(partitum, base_path) {
  out <- list()
  if (!is.null(partitum$examples)) {      
    ex <- partitum$examples
    ex <- gsub("([%\\])", "\\\\\\1", ex)
    ex <- gsub("\\\\dont", "\\dont", ex)
    out <- c(out, new_tag("examples", ex))
  } 
  
  paths <- unlist(partitum[names(partitum) == "example"])
  if (length(paths) > 0) {
    paths <- file.path(base_path, str_trim(paths))
    examples <- unlist(lapply(paths, readLines))
    examples <- gsub("([%\\])", "\\\\\\1", examples)                        
    
    out <- c(out, new_tag("examples", examples))
  }
  out
}
process.section <- function(key, value) {
  pieces <- str_split_fixed(value, ":", n = 2)[1, ]
  
  new_tag("section", list(list(name = pieces[1], content = pieces[2])))
}

process.docType <- function(partitum) {
  doctype <- partitum$docType %||% partitum$src_type
  
  if (is.null(doctype)) return()
  
  tags <- list()
  
  if (doctype == "package") {
    name <- partitum$name
    tags <- c(tags, new_tag("docType", "package"))
    if (!str_detect(name, "-package")) {
      tags <- c(tags, new_tag("alias", str_c(name, "-package")))
    }
  } else if (doctype == "data") {
    tags <- c(tags, new_tag("docType", "data"))
    if (is.null(partitum$format)) {
      tags <- c(tags, new_tag("format", partitum$str))
    }
    if (is.null(partitum$usage)) {
      tags <- c(tags, new_tag("usage", partitum$src_name))
    }
    tags <- c(tags, new_tag("keyword", "datasets"))
  }
  
  tags
}

process_had_tag <- function(partitum, tag, f = new_tag) {
  matches <- partitum[names(partitum) == tag]
  if (length(matches) == 0) return()

  unlist(lapply(matches, function(p) f(tag, p)), recursive = FALSE)
}

# warning("All roxygen elements must have name: ",
#   partitum$srcref$filename, ":", partitum$srcref$lloc[1], ":",
#   partitum$srcref$lloc[2], call. = FALSE)