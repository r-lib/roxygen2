The following notes were generated across my local OS X install, ubuntu running on travis-ci and win builder. Response to NOTEs across three platforms below.

* checking CRAN incoming feasibility ... NOTE
  Possibly mis-spelled words in DESCRIPTION:
  Doxygen (3:16), NAMESPACE (4:28)
  
  These are not spelling mistakes.

I also checked all downstream dependencies (even thought roxygen2 is a build-time, not run-time depency), https://github.com/wch/checkresults/blob/master/roxygen2/r-release/00check-summary.txt. Two packages failed R CMD check:

* nscancor: looks like a dependency mispecifiation issue, not related to 
  roxygen2
  
* scholar: looks like an intermittent HTTP failure, not related to roxygen.

Roxygen2 is used at package build time and should not have any run-time dependencies.
