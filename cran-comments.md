## Test environments

* local: darwin15.6.0-3.6.0
* travis: 3.1, 3.2, 3.3, oldrel, release, devel
* r-hub: windows-x86_64-devel, ubuntu-gcc-release, fedora-clang-devel
* win-builder: windows-x86_64-devel

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## revdepcheck results

I did not run revdep checks because this is a minor patch that only affects escaping of examples in a handful of cases.
