# attachment

<details>

* Version: 0.2.4
* GitHub: https://github.com/Thinkr-open/attachment
* Source code: https://github.com/cran/attachment
* Date/Publication: 2021-11-16 08:40:08 UTC
* Number of recursive dependencies: 53

Run `cloud_details(, "attachment")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [-] 3 package(s) removed: magrittr, utils, testthat.
      [+] 4 package(s) added: bookdown, fakeinstalled, glue, pagedown.
      Package(s) Rcpp is(are) in category 'LinkingTo'. Check your Description file to be sure it is really what you want.
      [ FAIL 1 | WARN 1 | SKIP 1 | PASS 116 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-att_from_namespace.R:88:3): bad namespace can be corrected ────
      `att_from_namespace(...)` did not throw the expected error.
      
      [ FAIL 1 | WARN 1 | SKIP 1 | PASS 116 ]
      Error: Test failures
      Execution halted
    ```

# DataPackageR

<details>

* Version: 0.15.8
* GitHub: https://github.com/ropensci/DataPackageR
* Source code: https://github.com/cran/DataPackageR
* Date/Publication: 2021-03-17 09:50:03 UTC
* Number of recursive dependencies: 90

Run `cloud_details(, "DataPackageR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
          extra.rmd:
            enabled: yes
        objects: cars_over_20
        render_root: dummy
      
      subsetCars.Rmd extra.rmd
      cars_over_20[ FAIL 1 | WARN 0 | SKIP 0 | PASS 208 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-document.R:49:3): documentation is built via document() ───────
      `document(file.path(tempdir(), "subsetCars"), lib = temp_libpath)` produced no output
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 208 ]
      Error: Test failures
      Execution halted
    ```

# roclang

<details>

* Version: 0.1.4
* GitHub: https://github.com/zhuxr11/roclang
* Source code: https://github.com/cran/roclang
* Date/Publication: 2022-02-01 16:00:05 UTC
* Number of recursive dependencies: 56

Run `cloud_details(, "roclang")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 43 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-extract_roc_text.R:44:3): extract_roc_text uses character function name input ──
      extract_roc_text\("library", "param", "package", NA\) does not match "^the name of a package".
      Actual value: "<NA>"
      Backtrace:
          ▆
       1. └─testthat::expect_match(...) at test-extract_roc_text.R:44:2
       2.   └─testthat:::expect_match_(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 43 ]
      Error: Test failures
      Execution halted
    ```

# tidyHeatmap

<details>

* Version: 1.6.0
* GitHub: https://github.com/stemangiola/tidyHeatmap
* Source code: https://github.com/cran/tidyHeatmap
* Date/Publication: 2022-01-28 17:20:02 UTC
* Number of recursive dependencies: 109

Run `cloud_details(, "tidyHeatmap")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction.Rmd’ using rmarkdown
    Could not fetch https://joss.theoj.org/papers/10.21105/joss.02472/status.svg
    HttpExceptionRequest Request {
      host                 = "joss.theoj.org"
      port                 = 443
      secure               = True
      requestHeaders       = []
      path                 = "/papers/10.21105/joss.02472/status.svg"
      queryString          = ""
    ...
     ConnectionTimeout
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 61
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

