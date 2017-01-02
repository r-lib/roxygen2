## Test environments
* local OS X install, R 3.2.3
* ubuntu 12.04 (on travis-ci), R 3.2.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

* I have run R CMD check on all five reverse dependencies of roxygen2 
  (https://github.com/klutometis/roxygen/tree/master/revdep). 

  I did not check packages that only suggest roxygen2, as it's generally a 
  build-time, rather a run-time dependency.

* There were no errors.
