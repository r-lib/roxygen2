# epivizr

<details>

* Version: 2.14.0
* Source code: https://github.com/cran/epivizr
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 114

Run `revdep_details(,"epivizr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(epivizr)
      > 
      > test_check("epivizr")
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘minfi’
    ```

# spocc

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/spocc
* URL: https://github.com/ropensci/spocc (devel), https://ropensci.github.io/spocc/ (user manual)
* BugReports: https://github.com/ropensci/spocc/issues
* Date/Publication: 2019-11-02 09:30:02 UTC
* Number of recursive dependencies: 119

Run `revdep_details(,"spocc")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      4: lapply(x, lazy_eval, data = data)
      5: FUN(X[[i]], ...)
      6: eval(x$expr, x$env, emptyenv())
      7: eval(x$expr, x$env, emptyenv())
      8: occ("Accipiter striatus", from = "ecoengine", limit = 5, has_coords = TRUE) at testthat/test-has_coords.R:22
      9: lapply(query, loopfun, y = limit, s = start, p = page, z = geometry, hc = has_coords, d = date, w = callopts) at /Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/spocc/new/spocc.Rcheck/00_pkg_src/spocc/R/occ.r:146
      10: FUN(X[[i]], ...)
      11: foo_ecoengine(sources, x, y, p, z, hc, d, w, ecoengineopts) at /Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/spocc/new/spocc.Rcheck/00_pkg_src/spocc/R/occ.r:68
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════════════════
      [ OK: 112 | SKIPPED: 9 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: has_coords works with all data sources as planned (@test-has_coords.R#21) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# zoon

<details>

* Version: 0.6.3
* Source code: https://github.com/cran/zoon
* URL: https://github.com/zoonproject/zoon
* BugReports: https://github.com/zoonproject/zoon/issues
* Date/Publication: 2018-01-23 17:35:04
* Number of recursive dependencies: 92

Run `revdep_details(,"zoon")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      Loading required package: raster
      Loading required package: sp
      [31m──[39m [31m1. Error: Check returns list with all the names (@testZoonCitation.R#13) [39m [31m─────────────────────────────[39m
      wrong arguments for subsetting an environment
      1: ZoonCitation("LogisticRegression") at testthat/testZoonCitation.R:13
      2: ZoonModuleParse(ModuleURL) at /Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/zoon/new/zoon.Rcheck/00_pkg_src/zoon/R/ZoonCitation.R:24
      3: lapply(names(rd), function(field, x) x[[1]][[field]]$values, rd) at /Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/zoon/new/zoon.Rcheck/00_pkg_src/zoon/R/ZoonModuleParse.R:59
      4: FUN(X[[i]], ...)
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════════════════
      [ OK: 203 | SKIPPED: 30 | WARNINGS: 36 | FAILED: 1 ]
      1. Error: Check returns list with all the names (@testZoonCitation.R#13) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

