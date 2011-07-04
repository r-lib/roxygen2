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

* clear caches function

# Reducing duplication

* @family tag for automatically generating seealso for related functions?
  Would create seealso that listed all functions except the current function
  in that family.  Functions could belong to multiple families.
  
        @family a
        @family b
        ->
        \seealso{Other related functions of family "a": \code{\link{x}}, y, z}
        
  Would require parse through all partitum (caching issues?)

* @inherit tag to automatically inherit tags from another topic? @inherit
  param just to inherit matching parameters?

* Templates, evaluated with brew

        @template templateName
        @templateVar name String mapped to variable name
        @templateVar value String mapped to variable value.

  If you wanted to use multiple templates, you'd need to ensure that their
  parameters had different names - not a huge problem in practice. @template
  tags in templates will be ignored.
  
  For plyr would have:
  
        @template in-d
        @template out-d
        @template ply
        
  Template in-d might look like:
  
        @param .data data frame to be processed
        @param .variables variables to split data frame by, as quoted
          variables, a formula or character vector
        @description  This function splits data frames by variables.
        @family data frame input
        
  Template out-d might look like:
  
        @description  The results into a list.  If there are no results,
          then this function will return a list of length 0  (\code{list()}).
        @return if results are atomic with same type and dimensionality, a
           vector, matrix or array; otherwise, a list-array (a list with
           dimensions)
        @family data frame output
     
  And lastly template ply:
  
        @param .fun function to apply to each piece
        @param ... other arguments passed on to \code{.fun}
        @param .progress name of the progress bar to use, see
          \code{\link{create_progress_bar}}
        @export
        @references Hadley Wickham (2011). The Split-Apply-Combine Strategy 
          for Data Analysis. Journal of Statistical Software, 40(1), 1-29. 
          \url{http://www.jstatsoft.org/v40/i01/}.   
  
  Should other parameters from the documentation be passed in?
  
  Where to store? man/? man-templates/? roxygen/? 
  What file extension? .R? .brew? .txt? 

* Suggests that we may want user specifiable plugins for rd_roclet (like there
  were before!) that you can choose between.
