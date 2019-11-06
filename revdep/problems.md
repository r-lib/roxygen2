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
      [31m──[39m [31m1. Error: Check returns list with all the names (@testZoonCitation.R#13) [39m [31m────────────────────────────────────────────────[39m
      wrong arguments for subsetting an environment
      1: ZoonCitation("LogisticRegression") at testthat/testZoonCitation.R:13
      2: ZoonModuleParse(ModuleURL) at /Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/zoon/new/zoon.Rcheck/00_pkg_src/zoon/R/ZoonCitation.R:24
      3: lapply(names(rd), function(field, x) x[[1]][[field]]$values, rd) at /Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/zoon/new/zoon.Rcheck/00_pkg_src/zoon/R/ZoonModuleParse.R:59
      4: FUN(X[[i]], ...)
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 203 | SKIPPED: 30 | WARNINGS: 35 | FAILED: 1 ]
      1. Error: Check returns list with all the names (@testZoonCitation.R#13) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

