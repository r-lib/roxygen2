## Test environments
* local OS X install, R 3.2.3
* ubuntu 12.04 (on travis-ci), R 3.2.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## revdepcheck results

We checked 238 reverse dependencies (213 from CRAN + 25 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 8 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* rstantools
  checking tests ...

### Failed to check

* dotCall64 (failed to install)
* dynr      (failed to install)
* jqr       (failed to install)
* OpenMx    (check timed out)
* redland   (failed to install)
* rMouse    (failed to install)
* rpf       (failed to install)
* rstanarm  (check timed out)
