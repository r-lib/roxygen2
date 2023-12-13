# attachment

<details>

* Version: 0.4.0
* GitHub: https://github.com/ThinkR-open/attachment
* Source code: https://github.com/cran/attachment
* Date/Publication: 2023-05-31 17:10:02 UTC
* Number of recursive dependencies: 59

Run `revdepcheck::cloud_details(, "attachment")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(attachment)
      > 
      > test_check("attachment")
      Saving attachment parameters to yaml config file
      Loading required namespace: rstudioapi
      Updating dummypackage documentation
    ...
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-att_from_namespace.R:92:5'): bad namespace can be corrected ──
      `att_amend_desc(dummypackage)` did not throw the expected warning.
      ── Failure ('test-att_from_namespace.R:109:3'): bad namespace can be corrected ──
      `att_from_namespace(...)` did not throw the expected warning.
      
      [ FAIL 2 | WARN 0 | SKIP 2 | PASS 222 ]
      Error: Test failures
      Execution halted
    ```

# checkhelper

<details>

* Version: 0.1.0
* GitHub: https://github.com/ThinkR-open/checkhelper
* Source code: https://github.com/cran/checkhelper
* Date/Publication: 2023-06-21 20:00:02 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "checkhelper")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(checkhelper)
      > 
      > test_check("checkhelper")
      Package: checkpackage
      Title: What the Package Does (One Line, Title Case)
      Version: 0.0.0.9000
    ...
          ▆
       1. ├─usethis::with_project(...) at test-find_missing_values.R:68:3
       2. │ └─base::force(code)
       3. └─testthat::expect_warning(attachment::att_amend_desc(), regexp = "@return") at test-find_missing_values.R:113:5
      ── Failure ('test-find_missing_values.R:120:5'): find_missing_tags works ───────
      `out <- expect_message(find_missing_tags(path), "my_long_fun_name_for_multiple_lines_globals")` did not produce any warnings.
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 67 ]
      Error: Test failures
      Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(checkhelper)
      > 
      > test_check("checkhelper")
      Package: checkpackage
      Title: What the Package Does (One Line, Title Case)
      Version: 0.0.0.9000
    ...
          ▆
       1. ├─usethis::with_project(...) at test-find_missing_values.R:68:3
       2. │ └─base::force(code)
       3. └─testthat::expect_warning(attachment::att_amend_desc(), regexp = "@return") at test-find_missing_values.R:113:5
      ── Failure ('test-find_missing_values.R:120:5'): find_missing_tags works ───────
      `out <- expect_message(find_missing_tags(path), "my_long_fun_name_for_multiple_lines_globals")` did not produce any warnings.
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 67 ]
      Error: Test failures
      Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(checkhelper)
      > 
      > test_check("checkhelper")
      Package: checkpackage
      Title: What the Package Does (One Line, Title Case)
      Version: 0.0.0.9000
    ...
          ▆
       1. ├─usethis::with_project(...) at test-find_missing_values.R:68:3
       2. │ └─base::force(code)
       3. └─testthat::expect_warning(attachment::att_amend_desc(), regexp = "@return") at test-find_missing_values.R:113:5
      ── Failure ('test-find_missing_values.R:120:5'): find_missing_tags works ───────
      `out <- expect_message(find_missing_tags(path), "my_long_fun_name_for_multiple_lines_globals")` did not produce any warnings.
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 67 ]
      Error: Test failures
      Execution halted
    ```

# doctest

<details>

* Version: 0.2.0
* GitHub: https://github.com/hughjonesd/doctest
* Source code: https://github.com/cran/doctest
* Date/Publication: 2023-04-28 17:50:05 UTC
* Number of recursive dependencies: 65

Run `revdepcheck::cloud_details(, "doctest")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
      `{ ... }` did not throw the expected warning.
      ── Failure ('test-variations.R:69:3'): Empty @testRaw tag ──────────────────────
      `{ ... }` did not throw the expected warning.
      
      [ FAIL 5 | WARN 0 | SKIP 13 | PASS 9 ]
      Deleting unused snapshots:
      • rd-roclet/palindrome.Rd
      • rd-roclet/safe_mean.Rd
      Error: Test failures
      Execution halted
    ```

