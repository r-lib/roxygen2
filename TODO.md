* Write S4 test cases:
  generic documentation
  class documentation
  method documentation - individual, class, generic

* Redocument roclet_rd()

* In roxygenise, run roclets that don't need run time code first - that way
  collate can be run first to figure out the order in which the files should
  be loaded

* Figure out how to cache parsing code, given that it now depends on the state
  of source.

* Figure out how to cache processing to avoid duplication when nothing has
  changed.