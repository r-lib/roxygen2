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
      [ FAIL 1 | WARN 0 | SKIP 1 | PASS 116 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-att_from_namespace.R:88:3): bad namespace can be corrected ────
      `att_from_namespace(...)` did not throw the expected error.
      
      [ FAIL 1 | WARN 0 | SKIP 1 | PASS 116 ]
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

*   checking examples ... ERROR
    ```
    Running examples in ‘roclang-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: extract_roc_text
    > ### Title: Extract a section, parameter or set of dot-parameters from a
    > ###   function documentation
    > ### Aliases: extract_roc_text
    > 
    > ### ** Examples
    > 
    ...
     11. │           ├─base::unlist(map2(args, docs, arg_matches))
     12. │           └─purrr::map2(args, docs, arg_matches)
     13. │             └─roxygen2 .f(.x[[1L]], .y[[1L]], ...)
     14. │               └─purrr::map_lgl(docs, function(x) x$name %in% args)
     15. ├─purrr::pluck(., "my_fun.Rd")
     16. └─purrr:::stop_bad_element_vector(...)
     17.   └─purrr:::stop_bad_vector(...)
     18.     └─purrr:::stop_bad_type(...)
     19.       └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       13. │       └─topics$apply(inherit_dot_params, env = env)
       14. │         └─roxygen2 fun(topic, self, ...)
       15. │           ├─base::unlist(map2(args, docs, arg_matches))
       16. │           └─purrr::map2(args, docs, arg_matches)
       17. │             └─roxygen2 .f(.x[[1L]], .y[[1L]], ...)
       18. │               └─purrr::map_lgl(docs, function(x) x$name %in% args)
       19. ├─purrr::pluck(., "my_fun.Rd")
       20. └─purrr:::stop_bad_element_vector(...)
       21.   └─purrr:::stop_bad_vector(...)
       22.     └─purrr:::stop_bad_type(...)
       23.       └─rlang::abort(...)
      
      [ FAIL 5 | WARN 0 | SKIP 0 | PASS 38 ]
      Error: Test failures
      Execution halted
    ```

# zmisc

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/zmisc
* Date/Publication: 2022-02-02 08:30:02 UTC
* Number of recursive dependencies: 56

Run `cloud_details(, "zmisc")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘zmisc.Rmd’ using rmarkdown
    Quitting from lines 57-69 (zmisc.Rmd) 
    Error: processing vignette 'zmisc.Rmd' failed with diagnostics:
    cannot unclass an environment
    --- failed re-building ‘zmisc.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘zmisc.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

