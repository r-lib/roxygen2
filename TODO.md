* Write S4 test cases:
  generic documentation
  class documentation
  method documentation - individual, class, generic

* In roxygenise, run roclets that don't need run time code first - that way
  collate can be run first to figure out the order in which the files should
  be loaded

* if @doctType package, automatically create the correct aliases

* if @docType data, automatically add dataset keyword, format and usage.

# Reducing duplication

* @family tag for automatically generating seealso for related functions?

* @inherit tag to automatically inherit tags from another topic?

