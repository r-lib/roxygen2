## Submission summary
This fixes a number of problems when running roxygen2 under R 3.1.0.

## Test environments
* local OS X install, R 3.2.1
* ubuntu 12.04 (on travis-ci), R 3.2.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

* I have run R CMD check on all five reverse dependencies of roxygen2 
  (https://github.com/hadley/roxygen2/blob/master/revdep/summary.md). 

  I did not check packages that only suggest roxygen2, as it's generally a 
  build-time, rather a run-time dependency.

* There were no errors.
