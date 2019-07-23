## Test environments
* OS X install: R 3.4
* win-builder: R-devel
* travis-ci: R 3.2, R 3.3, R 3.4, R 3.5, R-devel

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## revdepcheck results

We checked 258 reverse dependencies (233 from CRAN + 25 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 16 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* prioritizr
  checking examples ... ERROR
  
  Error related to socketConnection, not changes to roxygen2

### Failed to check

* circumplex       (failed to install)
* deisotoper       (failed to install)
* dotCall64        (failed to install)
* dynr             (failed to install)
* forecastHybrid   (failed to install)
* jqr              (failed to install)
* mapsRinteractive (failed to install)
* mgarchBEKK       (failed to install)
* OpenMx           (failed to install)
* redland          (failed to install)
* rMouse           (failed to install)
* rpcdsearch       (failed to install)
* rpf              (failed to install)
* rstanarm         (check timed out)
* SurfaceTortoise  (failed to install)
* vortexR          (failed to install)
