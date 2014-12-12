## Test environments
* local OS X install, R 3.1.2
* ubuntu 12.04 (on travis-ci), R 3.1.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Possibly mis-spelled words in DESCRIPTION:
  Doxygen (3:16), NAMESPACE (4:28)

  These are not spelling mistakes.

## Downstream dependencies
I have also run R CMD check on reverse dependencies of httr 
(https://github.com/hadley/roxygen2/blob/master/revdep/summary.md). I did not check packages that only suggest roxygen2, as it's generally a build-time, rather a run-time dependency.
