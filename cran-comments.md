This is a patch release to fix a major bug that affects a small percentage of users

---

## Test environments
* local OS X install, R 3.2.3
* ubuntu 12.04 (on travis-ci), R 3.2.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

I have run R CMD check on all 7 reverse dependencies of roxygen2 
I did not check packages that only suggest roxygen2, as it's generally a 
build-time, rather a run-time dependency.

Results summary at https://github.com/klutometis/roxygen/tree/master/revdep

I failed to install redland, but otherwise there were no new problems.
