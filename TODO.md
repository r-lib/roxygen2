* Write S4 test cases:
  generic documentation
  class documentation
  method documentation - individual, class, generic

* In roxygenise, run roclets that don't need run time code first - that way
  collate can be run first to figure out the order in which the files should
  be loaded

* @method should automatically inherit parameter documentation from generics
  (if available)?

* if @docType package, automatically create the correct aliases

* if @docType data, automatically add dataset keyword, format and usage.

# Reducing duplication

* @family tag for automatically generating seealso for related functions?
  Would create seealso that listed all functions except the current function
  in that family.  Functions could belong to multiple families.
  
        @family a
        @family b
        ->
        \seealso{Other related functions of family "a": \code{\link{x}}, y, z}
        
  Would require parse through all partitum (caching issues?)

* Start with `@inheritParam package::functionname` or `@inheritParam topic`.  

  Works either with roxygen topic in current package, or Rd for topic in
  another package.  Determined based on whether :: used in the parameter name.
  
  For roxygen topic:
    * need to topologically sort topics.
    * requires that names be preprocessed
    * (and adds another caching dependency)
  
  For Rd topic:
    * need to find file for that topic
    * need to parse file and extract list of arguments.
  
  Eventually need to do this automatically for methods: inherit from generic.

* Suggests that we may want user specifiable plugins for rd_roclet (like there
  were before!) that you can choose between.
