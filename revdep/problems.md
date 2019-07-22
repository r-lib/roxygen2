# ActisoftR

Version: 0.0.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 29-30 (ActisoftR.Rmd) 
    Error: processing vignette 'ActisoftR.Rmd' failed with diagnostics:
    there is no package called 'devtools'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# anomalize

Version: 0.1.1

## In both

*   checking examples ... ERROR
    ```
    ...
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > 
    > # Needed to pass CRAN check / This is loaded by default
    > set_time_scale_template(time_scale_template())
    > 
    > data(tidyverse_cran_downloads)
    > 
    > tidyverse_cran_downloads %>%
    +     time_decompose(count, method = "stl") %>%
    +     anomalize(remainder, method = "iqr")
    Error in mutate_impl(.data, dots) : 
      Evaluation error: Error in prep_tbl_time(): No date or datetime column found..
    Calls: %>% ... <Anonymous> -> <Anonymous> -> mutate.tbl_df -> mutate_impl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════
      OK: 21 SKIPPED: 0 FAILED: 24
      1. Error: iqr_grouped_df works (@test-anomalize.R#32) 
      2. Error: gesd_grouped_df works (@test-anomalize.R#45) 
      3. Error: returns a ggplot (@test-plot_anomalies.R#8) 
      4. Error: returns a ggplot (@test-plot_anomaly_decomposition.R#10) 
      5. Error: converts tibble to tbl_time (@test-prep_tbl_time.R#14) 
      6. Error: single tbl_df (@test-time_decompose.R#14) 
      7. Error: grouped tbl_df (@test-time_decompose.R#26) 
      8. Error: method = stl, auto freq/trend (@test-time_decompose.R#36) 
      9. Error: method = stl, character freq/trend (@test-time_decompose.R#46) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 15-24 (anomalize_methods.Rmd) 
    Error: processing vignette 'anomalize_methods.Rmd' failed with diagnostics:
    there is no package called 'devtools'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        help   4.7Mb
    ```

# aRxiv

Version: 0.5.16

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# ashr

Version: 2.2-7

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘REBayes’ ‘Rmosek’
    ```

# autovarCore

Version: 1.0-4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rcpp’ ‘jsonlite’
      All declared Imports should be used.
    ```

# bacon

Version: 1.8.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    .hist: no visible binding for global variable ‘..density..’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/bacon/new/bacon.Rcheck/00_pkg_src/bacon/R/BaconClass.R:329)
    .qq: no visible binding for global variable ‘column’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/bacon/new/bacon.Rcheck/00_pkg_src/bacon/R/BaconClass.R:352)
    Undefined global functions or variables:
      ..density.. column
    ```

# broman

Version: 0.68-2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘devtools’
    ```

# cartools

Version: 0.1.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘devtools’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# CDECRetrieve

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘lazyeval’ ‘purrr’ ‘roxygen2’
      All declared Imports should be used.
    ```

# CHARGE

Version: 1.0.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘CHARGE-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: exprFinder
    > ### Title: exprFinder
    > ### Aliases: exprFinder
    > 
    > ### ** Examples
    > 
    > library(GenomicRanges)
    > library(EnsDb.Hsapiens.v86)
    Error in library(EnsDb.Hsapiens.v86) : 
      there is no package called ‘EnsDb.Hsapiens.v86’
    Execution halted
    ```

*   checking running R code from vignettes ...
    ```
       ‘CHARGE_Vignette.Rnw’ using ‘UTF-8’ ... failed
     WARNING
    Errors in running code in vignettes:
    when running code in ‘CHARGE_Vignette.Rnw’
      ...
    The following objects are masked from ‘package:base’:
    
        aperm, apply
    
    
    > library(EnsDb.Hsapiens.v86)
    
      When sourcing ‘CHARGE_Vignette.R’:
    Error: there is no package called ‘EnsDb.Hsapiens.v86’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘EnsDb.Hsapiens.v86’
    ```

*   checking R code for possible problems ... NOTE
    ```
    exprFinder: no visible global function definition for ‘as’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/CHARGE/new/CHARGE.Rcheck/00_pkg_src/CHARGE/R/exprFinder.R:136)
    Undefined global functions or variables:
      as
    Consider adding
      importFrom("methods", "as")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
    
        anyMissing, rowMedians
    
    Loading required package: BiocParallel
    
    Attaching package: ‘DelayedArray’
    
    The following objects are masked from ‘package:matrixStats’:
    
        colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
    
    The following objects are masked from ‘package:base’:
    
        aperm, apply
    
    
    Error: processing vignette 'CHARGE_Vignette.Rnw' failed with diagnostics:
     chunk 1 (label = load the gene expression data) 
    Error in library(EnsDb.Hsapiens.v86) : 
      there is no package called ‘EnsDb.Hsapiens.v86’
    Execution halted
    ```

# charlatan

Version: 0.3.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        R     4.0Mb
        doc   1.0Mb
    ```

# chimeraviz

Version: 1.6.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    
    Depends: includes the non-default packages:
      ‘Biostrings’ ‘GenomicRanges’ ‘IRanges’ ‘Gviz’ ‘S4Vectors’ ‘ensembldb’
      ‘AnnotationFilter’ ‘data.table’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        doc       2.6Mb
        extdata   2.3Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    plot_fusion_transcript_with_protein_domain: no visible binding for
      global variable 'protein_domain_location'
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/chimeraviz/new/chimeraviz.Rcheck/00_pkg_src/chimeraviz/R/plot_fusion_transcript_with_protein_domain.R:523-532)
    plot_fusion_transcript_with_protein_domain: no visible binding for
      global variable 'protein_domain_location'
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/chimeraviz/new/chimeraviz.Rcheck/00_pkg_src/chimeraviz/R/plot_fusion_transcript_with_protein_domain.R:599-608)
    plot_fusion_transcript_with_protein_domain: no visible binding for
      global variable 'protein_domain_location'
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/chimeraviz/new/chimeraviz.Rcheck/00_pkg_src/chimeraviz/R/plot_fusion_transcript_with_protein_domain.R:764-773)
    plot_fusion_transcript_with_protein_domain: no visible binding for
      global variable 'protein_domain_location'
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/chimeraviz/new/chimeraviz.Rcheck/00_pkg_src/chimeraviz/R/plot_fusion_transcript_with_protein_domain.R:774-783)
    plot_fusion_transcript_with_protein_domain: no visible binding for
      global variable 'protein_domain_location'
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/chimeraviz/new/chimeraviz.Rcheck/00_pkg_src/chimeraviz/R/plot_fusion_transcript_with_protein_domain.R:784-793)
    Undefined global functions or variables:
      protein_domain_location
    ```

# chromer

Version: 0.1

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    parse_counts: no visible global function definition for ‘na.omit’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/chromer/new/chromer.Rcheck/00_pkg_src/chromer/R/clean-data.R:77)
    Undefined global functions or variables:
      na.omit
    Consider adding
      importFrom("stats", "na.omit")
    to your NAMESPACE file.
    ```

# circumplex

Version: 0.2.0

## In both

*   checking whether package ‘circumplex’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/circumplex/new/circumplex.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘circumplex’ ...
** package ‘circumplex’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/roxygen2/new/Rcpp/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/circumplex/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wall -g -O2 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘circumplex’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/circumplex/new/circumplex.Rcheck/circumplex’

```
### CRAN

```
* installing *source* package ‘circumplex’ ...
** package ‘circumplex’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/roxygen2/old/Rcpp/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/circumplex/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wall -g -O2 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘circumplex’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/circumplex/old/circumplex.Rcheck/circumplex’

```
# civis

Version: 1.5.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘devtools’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# clustermq

Version: 0.8.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘R6’ ‘infuser’ ‘purrr’
      All declared Imports should be used.
    ```

# codebook

Version: 0.6.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘graphics’ ‘pander’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 65 marked UTF-8 strings
    ```

# CompGLM

Version: 2.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             }
             lint(file, ..., parse_settings = FALSE)
         })
      7: FUN(X[[i]], ...)
      8: lint(file, ..., parse_settings = FALSE)
      9: get_source_expressions(filename)
      10: extract_r_source(source_file$filename, source_file$lines)
      11: grep(pattern$chunk.begin, lines, perl = TRUE)
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════
      OK: 18 SKIPPED: 0 FAILED: 1
      1. Error: check that package has google style (@test_code_style.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# CorShrink

Version: 0.1-6

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# CountClust

Version: 1.8.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 60-62 (count-clust.Rmd) 
    Error: processing vignette 'count-clust.Rmd' failed with diagnostics:
    there is no package called 'devtools'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.7Mb
      sub-directories of 1Mb or more:
        data   7.6Mb
    ```

# CrossClustering

Version: 4.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘glue’
      All declared Imports should be used.
    ```

# curatedMetagenomicData

Version: 1.10.2

## In both

*   checking examples ... ERROR
    ```
    ...
    > ###   HMP_2012.metaphlan_bugs_list.vagina
    > ###   HMP_2012.pathabundance_relab.nasalcavity
    > ###   HMP_2012.pathabundance_relab.oralcavity
    > ###   HMP_2012.pathabundance_relab.skin HMP_2012.pathabundance_relab.stool
    > ###   HMP_2012.pathabundance_relab.vagina HMP_2012.pathcoverage.nasalcavity
    > ###   HMP_2012.pathcoverage.oralcavity HMP_2012.pathcoverage.skin
    > ###   HMP_2012.pathcoverage.stool HMP_2012.pathcoverage.vagina
    > 
    > ### ** Examples
    > 
    > HMP_2012.metaphlan_bugs_list.nasalcavity()
    snapshotDate(): 2018-04-27
    see ?curatedMetagenomicData and browseVignettes('curatedMetagenomicData') for documentation
    downloading 0 resources
    loading from cache 
        ‘/Users/hadley//.ExperimentHub/1230’
    Error: failed to load resource
      name: EH1230
      title: 20180425.HMP_2012.metaphlan_bugs_list.nasalcavity
      reason: ReadItem: unknown type 108, perhaps written by later version of R
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        doc    1.4Mb
        help   2.7Mb
    ```

# datadr

Version: 0.8.6.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Rhipe’
    ```

# DataPackageR

Version: 0.15.4

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘devtools’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# deisotoper

Version: 0.0.3

## In both

*   checking whether package ‘deisotoper’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/deisotoper/new/deisotoper.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘deisotoper’ ...
** package ‘deisotoper’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/deisotoper/rJava/libs/rJava.so':
  dlopen(/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/deisotoper/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/deisotoper/rJava/libs/rJava.so
  Reason: image not found
Error : package ‘rJava’ could not be loaded
ERROR: lazy loading failed for package ‘deisotoper’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/deisotoper/new/deisotoper.Rcheck/deisotoper’

```
### CRAN

```
* installing *source* package ‘deisotoper’ ...
** package ‘deisotoper’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/deisotoper/rJava/libs/rJava.so':
  dlopen(/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/deisotoper/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/deisotoper/rJava/libs/rJava.so
  Reason: image not found
Error : package ‘rJava’ could not be loaded
ERROR: lazy loading failed for package ‘deisotoper’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/deisotoper/old/deisotoper.Rcheck/deisotoper’

```
# docstring

Version: 1.0.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# document

Version: 3.0.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# dodgr

Version: 0.1.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 18-19 (benchmark.Rmd) 
    Error: processing vignette 'benchmark.Rmd' failed with diagnostics:
    there is no package called 'devtools'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# doremi

Version: 0.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# dotCall64

Version: 1.0-0

## In both

*   checking whether package ‘dotCall64’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/dotCall64/new/dotCall64.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘dotCall64’ ...
** package ‘dotCall64’ successfully unpacked and MD5 sums checked
** libs
ccache clang -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -I../inst/include/ -DDOTCAL64_PRIVATE -fPIC  -Wall -g -O2  -c dotCall64.c -o dotCall64.o
clang: error: unsupported option '-fopenmp'
make: *** [dotCall64.o] Error 1
ERROR: compilation failed for package ‘dotCall64’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/dotCall64/new/dotCall64.Rcheck/dotCall64’

```
### CRAN

```
* installing *source* package ‘dotCall64’ ...
** package ‘dotCall64’ successfully unpacked and MD5 sums checked
** libs
ccache clang -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -I../inst/include/ -DDOTCAL64_PRIVATE -fPIC  -Wall -g -O2  -c dotCall64.c -o dotCall64.o
clang: error: unsupported option '-fopenmp'
make: *** [dotCall64.o] Error 1
ERROR: compilation failed for package ‘dotCall64’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/dotCall64/old/dotCall64.Rcheck/dotCall64’

```
# dr4pl

Version: 1.1.7.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# DrImpute

Version: 1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# dynr

Version: 0.1.13-4

## In both

*   checking whether package ‘dynr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/dynr/new/dynr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘dynr’ ...
** package ‘dynr’ successfully unpacked and MD5 sums checked
checking for gsl-config... no
configure: error: gsl-config not found, is GSL installed?
ERROR: configuration failed for package ‘dynr’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/dynr/new/dynr.Rcheck/dynr’

```
### CRAN

```
* installing *source* package ‘dynr’ ...
** package ‘dynr’ successfully unpacked and MD5 sums checked
checking for gsl-config... no
configure: error: gsl-config not found, is GSL installed?
ERROR: configuration failed for package ‘dynr’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/dynr/old/dynr.Rcheck/dynr’

```
# ecd

Version: 0.9.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘xts’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# edgarWebR

Version: 1.0.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# epiflows

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘htmlwidgets’
      All declared Imports should be used.
    ```

# epivizr

Version: 2.10.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
     9: timing_fn(handle(ev <- withCallingHandlers(withVisible(eval(expr,     envir, enclos)), warning = wHandler, error = eHandler, message = mHandler)))
    10: evaluate_call(expr, parsed$src[[i]], envir = envir, enclos = enclos,     debug = debug, last = i == length(out), use_try = stop_on_error !=         2L, keep_warning = keep_warning, keep_message = keep_message,     output_handler = output_handler, include_timing = include_timing)
    11: evaluate::evaluate(...)
    12: evaluate(code, envir = env, new_device = FALSE, keep_warning = !isFALSE(options$warning),     keep_message = !isFALSE(options$message), stop_on_error = if (options$error &&         options$include) 0L else 2L, output_handler = knit_handlers(options$render,         options))
    13: in_dir(input_dir(), evaluate(code, envir = env, new_device = FALSE,     keep_warning = !isFALSE(options$warning), keep_message = !isFALSE(options$message),     stop_on_error = if (options$error && options$include) 0L else 2L,     output_handler = knit_handlers(options$render, options)))
    14: block_exec(params)
    15: call_block(x)
    16: process_group.block(group)
    17: process_group(group)
    18: withCallingHandlers(if (tangle) process_tangle(group) else process_group(group),     error = function(e) {        setwd(wd)        cat(res, sep = "\n", file = output %n% "")        message("Quitting from lines ", paste(current_lines(i),             collapse = "-"), " (", knit_concord$get("infile"),             ") ")    })
    19: process_file(text, output)
    20: knitr::knit(knit_input, knit_output, envir = envir, quiet = quiet,     encoding = encoding)
    21: rmarkdown::render(file, encoding = encoding, quiet = quiet, envir = globalenv())
    22: vweave_rmarkdown(...)
    23: engine$weave(file, quiet = quiet, encoding = enc)
    24: doTryCatch(return(expr), name, parentenv, handler)
    25: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    26: tryCatchList(expr, classes, parentenv, handlers)
    27: tryCatch({    engine$weave(file, quiet = quiet, encoding = enc)    setwd(startdir)    find_vignette_product(name, by = "weave", engine = engine)}, error = function(e) {    stop(gettextf("processing vignette '%s' failed with diagnostics:\n%s",         file, conditionMessage(e)), domain = NA, call. = FALSE)})
    28: buildVignettes(dir = "/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/epivizr/new/epivizr.Rcheck/vign_test/epivizr")
    An irrecoverable exception occurred. R is aborting now ...
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘hgu133plus2.db’ ‘Mus.musculus’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘minfi’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Failed with error:  'package 'DelayedArray' could not be loaded'
      Error in .requirePackage(package) : 
        unable to find required package 'SummarizedExperiment'
      Calls: <Anonymous> ... getClass -> getClassDef -> .classEnv -> .requirePackage
      Execution halted
    ```

# epivizrChart

Version: 1.2.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      3: head(keys(hgu133plus2.db, keytype = "PROBEID"), nprobeids) at /Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/epivizrChart/new/epivizrChart.Rcheck/tests/testthat/helper-makeData.R:20
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════
      OK: 8 SKIPPED: 0 FAILED: 8
      1. Error: initializing charts works with data objects (@test-chart.R#5) 
      2. Error: initializing charts works with measurements (@test-chart.R#63) 
      3. Error: revisualizing charts as a different chart type works (@test-chart.R#80) 
      4. Error: navigating chart to different region works (@test-chart.R#91) 
      5. Error: initializing charts within an environment works (@test-env.R#5) 
      6. Error: removing charts from an environment works (@test-env.R#32) 
      7. Error: initializing charts within a navigation works (@test-nav.R#5) 
      8. Error: navigating charts within a navigation works (@test-nav.R#31) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    ...
        interactive
      Mismatches in argument names:
        Position: 5 Code: interactive Docs: ...
    
    Codoc mismatches from documentation object 'json_parser':
    json_parser
      Code: function(json_str, file, method = "C", unexpected.escape =
                     "error", simplify = TRUE)
      Docs: function(json_str, file, method = "C", unexpected.escape =
                     "error")
      Argument names in code not in docs:
        simplify
    
    Codoc mismatches from documentation object 'json_writer':
    json_writer
      Code: function(x, indent = 0, method = "C")
      Docs: function(x, method = "C")
      Argument names in code not in docs:
        indent
      Mismatches in argument names:
        Position: 2 Code: indent Docs: method
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 16-21 (IntroToEpivizrChart.Rmd) 
    Error: processing vignette 'IntroToEpivizrChart.Rmd' failed with diagnostics:
    there is no package called 'RColorBrewer'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘hgu133plus2.db’ ‘Mus.musculus’ ‘Homo.sapiens’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 15.8Mb
      sub-directories of 1Mb or more:
        data   2.2Mb
        doc    9.9Mb
        www    2.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: 'epivizrServer'
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘minfi’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Failed with error:  'package 'DelayedArray' could not be loaded'
      Error in .requirePackage(package) : 
        unable to find required package 'SummarizedExperiment'
      Calls: <Anonymous> ... getClass -> getClassDef -> .classEnv -> .requirePackage
      Execution halted
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    'library' or 'require' call not declared from: ‘RColorBrewer’
    ```

# epivizrData

Version: 1.8.0

## In both

*   checking whether package ‘epivizrData’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: next used in wrong context: no loop is visible at utils.R:90 
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/epivizrData/new/epivizrData.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘hgu133plus2.db’ ‘Mus.musculus’ ‘TxDb.Mmusculus.UCSC.mm10.knownGene’
      ‘EnsDb.Mmusculus.v79’
    ```

# eurostat

Version: 3.2.9

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    trying URL 'https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=data%2Ften00081.tsv.gz'
    Content type 'application/octet-stream;charset=UTF-8' length 13630 bytes (13 KB)
    ==================================================
    downloaded 13 KB
    
    Table ten00081 cached at /tmp/RtmpeJ7ak5/eurostat/ten00081_date_code_TF.rds
    trying URL 'https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=data%2Ftgs00026.tsv.gz'
    Content type 'application/octet-stream;charset=UTF-8' length 5998 bytes
    ==================================================
    downloaded 5998 bytes
    
    Quitting from lines 291-308 (eurostat_tutorial.Rmd) 
    Error: processing vignette 'eurostat_tutorial.Rmd' failed with diagnostics:
    Open failed.
    Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 596 marked UTF-8 strings
    ```

# exampletestr

Version: 1.4.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘devtools’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# fakemake

Version: 1.4.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘fakemake-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_pkg_archive_path
    > ### Title: Get a Package's Archive Path From the Package's DESCRIPTION
    > ### Aliases: get_pkg_archive_path
    > 
    > ### ** Examples
    > 
    > package_path <- file.path(tempdir(), "anRpackage")
    > usethis::create_package(path = package_path)
    [32m✔[39m Setting active project to [34m'/private/tmp/Rtmpqe3MG8/anRpackage'[39m
    [32m✔[39m Creating [34m'R/'[39m
    [32m✔[39m Creating [34m'man/'[39m
    [32m✔[39m Writing [34m'DESCRIPTION'[39m
    [32m✔[39m Writing [34m'NAMESPACE'[39m
    > print(tarball <- get_pkg_archive_path(package_path))
    Error in loadNamespace(name) : there is no package called ‘devtools’
    Calls: print ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Prerequisite DESCRIPTION found.
    Prerequisite R/throw.R found.
    Prerequisite R/throw.R found.
    Prerequisite R/throw.R found.
    Prerequisite R/throw.R found.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘pkgbuild’
      All declared Imports should be used.
    ```

# FedData

Version: 2.5.5

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘devtools’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# flippant

Version: 1.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# forecastHybrid

Version: 3.0.14

## In both

*   checking whether package ‘forecastHybrid’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/forecastHybrid/new/forecastHybrid.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘forecastHybrid’ ...
** package ‘forecastHybrid’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘forecast’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
 there is no package called ‘xts’
Error : package ‘forecast’ could not be loaded
ERROR: lazy loading failed for package ‘forecastHybrid’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/forecastHybrid/new/forecastHybrid.Rcheck/forecastHybrid’

```
### CRAN

```
* installing *source* package ‘forecastHybrid’ ...
** package ‘forecastHybrid’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘forecast’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
 there is no package called ‘xts’
Error : package ‘forecast’ could not be loaded
ERROR: lazy loading failed for package ‘forecastHybrid’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/forecastHybrid/old/forecastHybrid.Rcheck/forecastHybrid’

```
# fulltext

Version: 1.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        examples   4.2Mb
    ```

# gapfill

Version: 0.9.6

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘raster’ ‘doParallel’ ‘doMPI’
    ```

# GenVisR

Version: 1.12.1

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘GenVisR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Lolliplot-class
    > ### Title: Class Lolliplot
    > ### Aliases: Lolliplot-class Lolliplot
    > 
    > ### ** Examples
    > 
    > # Load a pre-existing data set
    > dataset <- PIK3CA
    > 
    > # mode 1, amino acid changes are not present
    > 
    > library(TxDb.Hsapiens.UCSC.hg38.knownGene)
    Error in library(TxDb.Hsapiens.UCSC.hg38.knownGene) : 
      there is no package called ‘TxDb.Hsapiens.UCSC.hg38.knownGene’
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [31m──[39m [31m3. Error: (unknown) (@test-genCov_qual.R#1) [39m [31m─────────────────────────────────────────────────────[39m
      there is no package called 'BSgenome.Hsapiens.UCSC.hg19'
      1: suppressPackageStartupMessages(library(BSgenome.Hsapiens.UCSC.hg19)) at testthat/test-genCov_qual.R:1
      2: withCallingHandlers(expr, packageStartupMessage = function(c) invokeRestart("muffleMessage"))
      3: library(BSgenome.Hsapiens.UCSC.hg19)
      4: stop(txt, domain = NA)
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════
      OK: 454 SKIPPED: 35 FAILED: 3
      1. Error: (unknown) (@test-Lolliplot-class.R#3) 
      2. Error: (unknown) (@test-VEP-class.R#306) 
      3. Error: (unknown) (@test-genCov_qual.R#1) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    genome specified is preloaded, retrieving data...
    Argument not supplied to target, defaulting to predefined identity SNPs from hg19 assembly!
    Obtaining CDS Coordinates
    This function is part of the new S4 feature and is under active development
    This function is part of the new S4 feature and is under active development
    Warning in .local(object, labelColumn, verbose, ...) :
      Removed 1212 rows from the data which harbored duplicate genomic locations
    Warning in .local(object, labelColumn, verbose, ...) :
      Removed 1212 rows from the data which harbored duplicate genomic locations
    Warning in .local(object, labelColumn, verbose, ...) :
      Removed 1212 rows from the data which harbored duplicate genomic locations
    Warning in .local(object, labelColumn, verbose, ...) :
      Removed 1212 rows from the data which harbored duplicate genomic locations
    Warning in setMutationHierarchy(object, mutationHierarchy, verbose) :
      The following mutations were found in the input however were not specified in the mutationHierarchy! upstream_gene_variant, splice_region_variant,non_coding_transcript_exon_variant, intron_variant,non_coding_transcript_variant, downstream_gene_variant, non_coding_transcript_exon_variant, 5_prime_UTR_variant, intron_variant, stop_lost, regulatory_region_variant, 3_prime_UTR_variant, intron_variant,NMD_transcript_variant, missense_variant,NMD_transcript_variant, inframe_insertion, inframe_deletion, frameshift_variant, 3_prime_UTR_variant,NMD_transcript_variant, splice_acceptor_variant,non_coding_transcript_variant, missense_variant,splice_region_variant,NMD_transcript_variant, synonymous_variant, synonymous_variant,NMD_transcript_variant, splice_donor_variant,non_coding_transcript_variant, start_lost, stop_gained,NMD_transcript_variant, 5_prime_UTR_variant,NMD_transcript_variant adding these in as least important and assigning random colors!
    Warning in .local(object, labelColumn, verbose, ...) :
      Removed 1212 rows from the data which harbored duplicate genomic locations
    Quitting from lines 224-233 (Upcoming_Features.Rmd) 
    Error: processing vignette 'Upcoming_Features.Rmd' failed with diagnostics:
    there is no package called 'BSgenome.Hsapiens.UCSC.hg19'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘BSgenome.Hsapiens.UCSC.hg19’ ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
      ‘TxDb.Hsapiens.UCSC.hg38.knownGene’ ‘BSgenome.Hsapiens.UCSC.hg38’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 16.2Mb
      sub-directories of 1Mb or more:
        R         3.0Mb
        doc      11.5Mb
        extdata   1.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘reshape2’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    setTierTwo,data.table : a: no visible binding for global variable ‘tmp’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/GenVisR/new/GenVisR.Rcheck/00_pkg_src/GenVisR/R/Lolliplot-class.R:969)
    toLolliplot,GMS: no visible binding for global variable ‘missingINdex’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/GenVisR/new/GenVisR.Rcheck/00_pkg_src/GenVisR/R/GMS-class.R:536-537)
    toLolliplot,VEP: no visible binding for global variable ‘missingINdex’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/GenVisR/new/GenVisR.Rcheck/00_pkg_src/GenVisR/R/VEP-class.R:933-934)
    Undefined global functions or variables:
      missingINdex tmp
    ```

# geometa

Version: 0.3-0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.9Mb
      sub-directories of 1Mb or more:
        R         6.0Mb
        extdata   1.3Mb
    ```

# getCRUCLdata

Version: 0.2.5

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(getCRUCLdata)
      > 
      > test_check("getCRUCLdata")
      [31m──[39m [31m1. Failure: Test that create_stack creates tmn if requested (@test-create_CRU_stack.R#1233) [39m [31m─────[39m
      raster::maxValue(CRU_stack_list[[1]][[1]]) not equal to 4.3.
      1/1 mismatches
      [1] 4.3 - 4.3 == 3.81e-07
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════
      OK: 326 SKIPPED: 23 FAILED: 1
      1. Failure: Test that create_stack creates tmn if requested (@test-create_CRU_stack.R#1233) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ggenealogy

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2356 marked UTF-8 strings
    ```

# gitlabr

Version: 0.9

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# googleAuthR

Version: 0.6.3

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘devtools’
    ```

# gqlr

Version: 0.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 28.2Mb
      sub-directories of 1Mb or more:
        R  28.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘base’
      All declared Imports should be used.
    ```

# Grid2Polygons

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘methods’ ‘raster’ ‘rgeos’ ‘sp’
      All declared Imports should be used.
    ```

# HMP16SData

Version: 1.0.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Attaching package: 'dendextend'
    
    The following object is masked from 'package:stats':
    
        cutree
    
    ========================================
    circlize version 0.4.4
    CRAN page: https://cran.r-project.org/package=circlize
    Github page: https://github.com/jokergoo/circlize
    Documentation: http://jokergoo.github.io/circlize_book/book/
    
    If you use it in published research, please cite:
    Gu, Z. circlize implements and enhances circular visualization 
      in R. Bioinformatics 2014.
    ========================================
    
    Quitting from lines 58-71 (HMP16SData.Rmd) 
    Error: processing vignette 'HMP16SData.Rmd' failed with diagnostics:
    there is no package called 'curatedMetagenomicData'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘curatedMetagenomicData’ ‘devtools’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 19.1Mb
      sub-directories of 1Mb or more:
        doc       1.5Mb
        extdata  17.4Mb
    ```

# icd

Version: 3.2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 23.2Mb
      sub-directories of 1Mb or more:
        R     16.2Mb
        doc    5.1Mb
        libs   1.1Mb
    ```

# inlmisc

Version: 0.4.3

## In both

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > r <- raster::raster(nrow = 10, ncol = 10, crs = NA)
    > r[] <- 1L
    > r[51:100] <- 2L
    > r[3:6, 1:5] <- 8L
    > r <- raster::ratify(r)
    > rat <- cbind(raster::levels(r)[[1]], land.cover = c("Pine", "Oak", "Meadow"))
    > levels(r) <- rat
    > PlotMap(r)
    > 
    > data(meuse, meuse.grid, package = "sp")
    > sp::coordinates(meuse.grid) <- ~x+y
    > sp::proj4string(meuse.grid) <- sp::CRS("+init=epsg:28992")
    > sp::gridded(meuse.grid) <- TRUE
    > meuse.grid <- raster::raster(meuse.grid, layer = "soil")
    > model <- gstat::gstat(id = "zinc", formula = zinc~1, locations = ~x+y, data = meuse)
    Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
      there is no package called ‘xts’
    Calls: :: ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘devtools’
    ```

# isdparser

Version: 0.3.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 104 marked UTF-8 strings
    ```

# jiebaR

Version: 0.9.99

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# jqr

Version: 1.1.0

## In both

*   checking whether package ‘jqr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/jqr/new/jqr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘jqr’ ...
** package ‘jqr’ successfully unpacked and MD5 sums checked
Using PKG_CFLAGS=-I/usr/local/opt/jq/include
Using PKG_LIBS=-L/usr/local/lib -ljq
------------------------- ANTICONF ERROR ---------------------------
Configuration failed because libjq was not found.
On Ubuntu 14.04 or 16.04 you can use the PPA:
  sudo add-apt-repository -y ppa:opencpu/jq
  sudo apt-get update
  sudo apt-get install libjq-dev
On other sytems try installing:
 * deb: libjq-dev (Debian, Ubuntu 16.10 and up).
 * rpm: jq-devel (Fedora, EPEL)
 * csw: libjq_dev (Solaris)
 * brew: jq (OSX)
If  is already installed set INCLUDE_DIR and LIB_DIR manually via:
R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
--------------------------------------------------------------------
ERROR: configuration failed for package ‘jqr’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/jqr/new/jqr.Rcheck/jqr’

```
### CRAN

```
* installing *source* package ‘jqr’ ...
** package ‘jqr’ successfully unpacked and MD5 sums checked
Using PKG_CFLAGS=-I/usr/local/opt/jq/include
Using PKG_LIBS=-L/usr/local/lib -ljq
------------------------- ANTICONF ERROR ---------------------------
Configuration failed because libjq was not found.
On Ubuntu 14.04 or 16.04 you can use the PPA:
  sudo add-apt-repository -y ppa:opencpu/jq
  sudo apt-get update
  sudo apt-get install libjq-dev
On other sytems try installing:
 * deb: libjq-dev (Debian, Ubuntu 16.10 and up).
 * rpm: jq-devel (Fedora, EPEL)
 * csw: libjq_dev (Solaris)
 * brew: jq (OSX)
If  is already installed set INCLUDE_DIR and LIB_DIR manually via:
R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
--------------------------------------------------------------------
ERROR: configuration failed for package ‘jqr’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/jqr/old/jqr.Rcheck/jqr’

```
# jubilee

Version: 0.2.5

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘xts’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# konfound

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 42-43 (Introduction_to_konfound.Rmd) 
    Error: processing vignette 'Introduction_to_konfound.Rmd' failed with diagnostics:
    there is no package called 'devtools'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# latticeDensity

Version: 1.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# lawn

Version: 0.4.2

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘maps’ ‘geojsonio’
    ```

# ldhmm

Version: 0.4.5

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘xts’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# learnrbook

Version: 0.0.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# lineup

Version: 0.37-10

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# link2GI

Version: 0.3-5

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘devtools’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# lmQCM

Version: 0.1.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# loose.rock

Version: 1.0.9

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 27-28 (Overview.Rmd) 
    Error: processing vignette 'Overview.Rmd' failed with diagnostics:
    there is no package called 'devtools'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘futile.options’ ‘ggfortify’ ‘grDevices’ ‘stats’
      All declared Imports should be used.
    ```

# mapsRinteractive

Version: 0.1.0

## In both

*   checking whether package ‘mapsRinteractive’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/mapsRinteractive/new/mapsRinteractive.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘mapsRinteractive’ ...
** package ‘mapsRinteractive’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘xts’
ERROR: lazy loading failed for package ‘mapsRinteractive’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/mapsRinteractive/new/mapsRinteractive.Rcheck/mapsRinteractive’

```
### CRAN

```
* installing *source* package ‘mapsRinteractive’ ...
** package ‘mapsRinteractive’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘xts’
ERROR: lazy loading failed for package ‘mapsRinteractive’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/mapsRinteractive/old/mapsRinteractive.Rcheck/mapsRinteractive’

```
# MAST

Version: 1.6.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 36-54 (MAITAnalysis.Rmd) 
    Error: processing vignette 'MAITAnalysis.Rmd' failed with diagnostics:
    there is no package called 'TxDb.Hsapiens.UCSC.hg19.knownGene'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 10.6Mb
      sub-directories of 1Mb or more:
        R      2.1Mb
        data   4.5Mb
        doc    3.7Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    collectResiduals: no visible global function definition for
      ‘assayNames<-’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/MAST/new/MAST.Rcheck/00_pkg_src/MAST/R/zlmHooks.R:57)
    primerAverage: no visible global function definition for ‘assay<-’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/MAST/new/MAST.Rcheck/00_pkg_src/MAST/R/Fluidigm-methods.R:314)
    primerAverage: no visible global function definition for ‘rowData<-’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/MAST/new/MAST.Rcheck/00_pkg_src/MAST/R/Fluidigm-methods.R:315)
    assay<-,SingleCellAssay-missing: no visible global function definition
      for ‘assay<-’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/MAST/new/MAST.Rcheck/00_pkg_src/MAST/R/SingleCellAssay-methods.R:574)
    assayNames<-,SingleCellAssay-character: no visible global function
      definition for ‘assayNames’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/MAST/new/MAST.Rcheck/00_pkg_src/MAST/R/SingleCellAssay-methods.R:412)
    assayNames<-,SingleCellAssay-character: no visible global function
      definition for ‘assays<-’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/MAST/new/MAST.Rcheck/00_pkg_src/MAST/R/SingleCellAssay-methods.R:418)
    exprs<-,SingleCellAssay-ANY: no visible global function definition for
      ‘assay<-’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/MAST/new/MAST.Rcheck/00_pkg_src/MAST/R/SingleCellAssay-methods.R:590)
    Undefined global functions or variables:
      assay<- assayNames assayNames<- assays<- rowData<-
    ```

# mgarchBEKK

Version: 0.0.2

## In both

*   checking whether package ‘mgarchBEKK’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/mgarchBEKK/new/mgarchBEKK.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

## Installation

### Devel

```
* installing *source* package ‘mgarchBEKK’ ...
** package ‘mgarchBEKK’ successfully unpacked and MD5 sums checked
** libs
ccache clang -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c loglikelihood.c -o loglikelihood.o
ccache clang -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c loglikelihood_GJR.c -o loglikelihood_GJR.o
ccache clang -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c matrixlib.c -o matrixlib.o
ccache clang -Qunused-arguments -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o mgarchBEKK.so loglikelihood.o loglikelihood_GJR.o matrixlib.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/mgarchBEKK/new/mgarchBEKK.Rcheck/mgarchBEKK/libs
** R
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘tseries’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
 there is no package called ‘xts’
Error : package ‘tseries’ could not be loaded
ERROR: lazy loading failed for package ‘mgarchBEKK’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/mgarchBEKK/new/mgarchBEKK.Rcheck/mgarchBEKK’

```
### CRAN

```
* installing *source* package ‘mgarchBEKK’ ...
** package ‘mgarchBEKK’ successfully unpacked and MD5 sums checked
** libs
ccache clang -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c loglikelihood.c -o loglikelihood.o
ccache clang -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c loglikelihood_GJR.c -o loglikelihood_GJR.o
ccache clang -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC  -Wall -g -O2  -c matrixlib.c -o matrixlib.o
ccache clang -Qunused-arguments -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o mgarchBEKK.so loglikelihood.o loglikelihood_GJR.o matrixlib.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/mgarchBEKK/old/mgarchBEKK.Rcheck/mgarchBEKK/libs
** R
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘tseries’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
 there is no package called ‘xts’
Error : package ‘tseries’ could not be loaded
ERROR: lazy loading failed for package ‘mgarchBEKK’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/mgarchBEKK/old/mgarchBEKK.Rcheck/mgarchBEKK’

```
# micompr

Version: 1.1.0

## In both

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘micompr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: assumptions.cmpoutput
    > ### Title: Get assumptions for parametric tests performed on output
    > ###   comparisons
    > ### Aliases: assumptions.cmpoutput
    > 
    > ### ** Examples
    > 
    > 
    > # Create a cmpoutput object from the provided datasets
    > cmp <- cmpoutput("All", 0.9, pphpc_ok$data[["All"]], pphpc_ok$obs_lvls)
    > 
    > # Get the assumptions for the parametric tests performed in cmp
    > acmp <- assumptions(cmp)
    sROC 0.1-2 loaded
    MANOVA assumptions require 'MVN' and 'biotools' packages.
    Error in `*tmp*`[[i]] : subscript out of bounds
    Calls: assumptions -> assumptions.cmpoutput
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      subscript out of bounds
      1: assumptions(mic1a) at testthat/test_micomp.R:281
      2: assumptions.micomp(mic1a) at /Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/micompr/new/micompr.Rcheck/00_pkg_src/micompr/R/assumptions.R:19
      3: lapply(obj, function(x) assumptions(x)) at /Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/micompr/new/micompr.Rcheck/00_pkg_src/micompr/R/micomp.R:498
      4: FUN(X[[i]], ...)
      5: assumptions(x) at /Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/micompr/new/micompr.Rcheck/00_pkg_src/micompr/R/micomp.R:498
      6: assumptions.cmpoutput(x) at /Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/micompr/new/micompr.Rcheck/00_pkg_src/micompr/R/assumptions.R:19
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════
      OK: 363 SKIPPED: 0 FAILED: 2
      1. Error: assumptions.cmpoutput creates the correct object (@test_cmpoutput.R#214) 
      2. Error: micomp assumptions have the correct properties (@test_micomp.R#281) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 341-342 (paper.Rnw) 
    Error: processing vignette 'paper.Rnw' failed with diagnostics:
    subscript out of bounds
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘biotools’ ‘devtools’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘biotools’
    ```

# mizer

Version: 1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(mizer)
      > 
      > test_check("mizer")
      [31m──[39m [31m1. Failure: retune_abundance reproduces scaling model (@test-wrapper_functions.R#17) [39m [31m────────────[39m
      max(abs(initial_n - pr@initial_n)) is not strictly less than 2e-11. Difference: 2.47e-11
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════
      OK: 508 SKIPPED: 1 FAILED: 1
      1. Failure: retune_abundance reproduces scaling model (@test-wrapper_functions.R#17) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘deSolve’ ‘progress’
      All declared Imports should be used.
    ```

# Momocs

Version: 1.2.9

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    This is Momocs 1.2.9
    
    Attaching package: 'Momocs'
    
    The following object is masked from 'package:stats':
    
        filter
    
    Quitting from lines 39-41 (Momocs_grindr.Rmd) 
    Error: processing vignette 'Momocs_grindr.Rmd' failed with diagnostics:
    there is no package called 'devtools'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        R   3.0Mb
    ```

# MoonlightR

Version: 1.6.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘TCGAbiolinks’
    
    Package suggested but not available for checking: ‘devtools’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# morpheus

Version: 0.2-0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# mregions

Version: 0.1.6

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘leaflet’
    ```

# msgbsR

Version: 1.4.0

## In both

*   checking examples ... ERROR
    ```
    ...
    
    The following objects are masked from 'package:Biobase':
    
        anyMissing, rowMedians
    
    Loading required package: BiocParallel
    
    Attaching package: 'DelayedArray'
    
    The following objects are masked from 'package:matrixStats':
    
        colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
    
    The following objects are masked from 'package:base':
    
        aperm, apply
    
    > library(BSgenome.Rnorvegicus.UCSC.rn6)
    Error in library(BSgenome.Rnorvegicus.UCSC.rn6) : 
      there is no package called 'BSgenome.Rnorvegicus.UCSC.rn6'
    Execution halted
    ```

*   checking data for ASCII and uncompressed saves ... WARNING
    ```
      
      Note: significantly better compression could be obtained
            by using R CMD build --resave-data
                   old_size new_size compress
      ratdata.rda     318Kb    125Kb       xz
      ratdata2.rda    287Kb    116Kb       xz
    ```

*   checking running R code from vignettes ...
    ```
       ‘msgbsR_Vignette.Rnw’ using ‘UTF-8’ ... failed
     WARNING
    Errors in running code in vignettes:
    when running code in ‘msgbsR_Vignette.Rnw’
      ...
    +     "+", yes = start(cutSites) - 1, no = start(cutSites) - 2)
    
    > end(cutSites) <- ifelse(test = strand(cutSites) == 
    +     "+", yes = end(cutSites) + 2, no = end(cutSites) + 1)
    
    > library(BSgenome.Rnorvegicus.UCSC.rn6)
    
      When sourcing 'msgbsR_Vignette.R':
    Error: there is no package called 'BSgenome.Rnorvegicus.UCSC.rn6'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘BSgenome.Rnorvegicus.UCSC.rn6’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 16.1Mb
      sub-directories of 1Mb or more:
        extdata  15.2Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: 'utils'
      All declared Imports should be used.
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
    
        anyMissing, rowMedians
    
    Loading required package: BiocParallel
    
    Attaching package: 'DelayedArray'
    
    The following objects are masked from 'package:matrixStats':
    
        colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
    
    The following objects are masked from 'package:base':
    
        aperm, apply
    
    
    Error: processing vignette 'msgbsR_Vignette.Rnw' failed with diagnostics:
     chunk 4 (label = run checkCuts with BSgenome) 
    Error in library(BSgenome.Rnorvegicus.UCSC.rn6) : 
      there is no package called 'BSgenome.Rnorvegicus.UCSC.rn6'
    Execution halted
    ```

# MSnbase

Version: 2.6.4

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘MSnbase-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: FeatComp-class
    > ### Title: Class '"FeatComp"'
    > ### Aliases: FeatComp-class compfnames-methods
    > ###   compfnames,MSnSet,MSnSet-method compfnames,list,missing-method
    > ###   compfnames show,FeatComp-method names,FeatComp-method
    > ###   common,FeatComp-method common,methods common unique1,FeatComp-method
    > ###   unique1,methods unique1 unique2,FeatComp-method unique2,methods
    > ###   unique2
    > ### Keywords: classes
    > 
    > ### ** Examples
    > 
    > library("pRolocdata")
    Error in library("pRolocdata") : there is no package called ‘pRolocdata’
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        mzR has been built against a different Rcpp version (0.12.16)
      than is installed on your system (0.12.19). This might lead to errors
      when loading mzR. If you encounter such issues, please send a report,
      including the output of sessionInfo() to the Bioc support forum at 
      https://support.bioconductor.org/. For details see also
      https://github.com/sneumann/mzR/wiki/mzR-Rcpp-compiler-linker-issue.
      > setMSnbaseVerbose(FALSE)
      > ## register(SerialParam()) ## see issue 205
      > 
      > ## Erwinia
      > f <- msdata::proteomics(full.names = TRUE,
      +                         pattern = "TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01.mzML.gz")
      Error in loadNamespace(name) : there is no package called 'msdata'
      Calls: :: ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Loading required package: BiocParallel
    Loading required package: ProtGenerics
    
    This is MSnbase version 2.6.4 
      Visit https://lgatto.github.io/MSnbase/ to get started.
    
    
    Attaching package: 'MSnbase'
    
    The following object is masked from 'package:stats':
    
        smooth
    
    The following object is masked from 'package:base':
    
        trimws
    
    Quitting from lines 65-72 (MSnbase-centroiding.Rmd) 
    Error: processing vignette 'MSnbase-centroiding.Rmd' failed with diagnostics:
    there is no package called 'msdata'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘pRolocdata’ ‘msdata’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 15.1Mb
      sub-directories of 1Mb or more:
        R      4.1Mb
        data   1.9Mb
        doc    7.8Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Authors@R field gives more than one person with maintainer role:
      Laurent Gatto <lg390@cam.ac.uk> [aut, cre]
      Johannes Rainer <Johannes.Rainer@eurac.edu> [aut, cre]
      Sebastian Gibb <mail@sebastiangibb.de> [aut, cre]
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘Biobase:::.showAnnotatedDataFrame’ ‘MALDIquant:::.estimateNoise’
      ‘MALDIquant:::.localMaxima’ ‘MALDIquant:::.movingAverage’
      ‘MALDIquant:::.savitzkyGolay’
      See the note in ?`:::` about the use of this operator.
    ```

# multipanelfigure

Version: 2.0.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘tiff’, ‘png’, ‘jpeg’, ‘rsvg’
    ```

# NanoStringQCPro

Version: 1.12.0

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘knitr’
    A package should be listed in only one of these fields.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    previewPNG: no visible global function definition for ‘png’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/NanoStringQCPro/new/NanoStringQCPro.Rcheck/00_pkg_src/NanoStringQCPro/R/previewPNG.R:38-42)
    previewPNG: no visible global function definition for ‘dev.off’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/NanoStringQCPro/new/NanoStringQCPro.Rcheck/00_pkg_src/NanoStringQCPro/R/previewPNG.R:61)
    sampleClustering,RccSet: no visible global function definition for
      ‘png’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/NanoStringQCPro/new/NanoStringQCPro.Rcheck/00_pkg_src/NanoStringQCPro/R/NanoStringQualityMetrics.R:1369)
    sampleClustering,RccSet: no visible global function definition for
      ‘dev.off’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/NanoStringQCPro/new/NanoStringQCPro.Rcheck/00_pkg_src/NanoStringQCPro/R/NanoStringQualityMetrics.R:1383)
    sampleClustering,RccSet: no visible global function definition for
      ‘png’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/NanoStringQCPro/new/NanoStringQCPro.Rcheck/00_pkg_src/NanoStringQCPro/R/NanoStringQualityMetrics.R:1390)
    sampleClustering,RccSet: no visible global function definition for
      ‘dev.off’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/NanoStringQCPro/new/NanoStringQCPro.Rcheck/00_pkg_src/NanoStringQCPro/R/NanoStringQualityMetrics.R:1405)
    Undefined global functions or variables:
      dev.off png
    Consider adding
      importFrom("grDevices", "dev.off", "png")
    to your NAMESPACE file.
    ```

# nima

Version: 0.5.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘devtools’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# nlmeU

Version: 0.70-3

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/nlmeU/new/nlmeU.Rcheck/00_pkg_src/nlmeU/R/logLik1.R:124)
    logLik1.lme: no visible global function definition for ‘formula’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/nlmeU/new/nlmeU.Rcheck/00_pkg_src/nlmeU/R/logLik1.R:127)
    runScript: no visible binding for global variable ‘example’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/nlmeU/new/nlmeU.Rcheck/00_pkg_src/nlmeU/R/varia.R:97)
    sigmaTolme: no visible global function definition for ‘vcov’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/nlmeU/new/nlmeU.Rcheck/00_pkg_src/nlmeU/R/simulateY.R:110)
    simulateY.lme: no visible global function definition for ‘runif’
    simulateY.lme: no visible global function definition for ‘fitted’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/nlmeU/new/nlmeU.Rcheck/00_pkg_src/nlmeU/R/simulateY.R:51)
    simulateY.lme: no visible global function definition for ‘rnorm’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/nlmeU/new/nlmeU.Rcheck/00_pkg_src/nlmeU/R/simulateY.R:57)
    Undefined global functions or variables:
      anova coef example fitted formula model.matrix pf predict qf rnorm
      runif vcov
    Consider adding
      importFrom("stats", "anova", "coef", "fitted", "formula",
                 "model.matrix", "pf", "predict", "qf", "rnorm", "runif",
                 "vcov")
      importFrom("utils", "example")
    to your NAMESPACE file.
    ```

# OpenMx

Version: 2.11.5

## In both

*   checking whether package ‘OpenMx’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/OpenMx/new/OpenMx.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘devtools’ ‘Rmpi’
    ```

## Installation

### Devel

```
* installing *source* package ‘OpenMx’ ...
** package ‘OpenMx’ successfully unpacked and MD5 sums checked
NOTE: ./configure is not an autoconf generated script.
Change default C/C++ compiler and default compile flags by editing ~/.R/Makevars
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/roxygen2/new/Rcpp/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/OpenMx/RcppEigen/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/OpenMx/StanHeaders/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/OpenMx/BH/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/OpenMx/rpf/include" -I/usr/local/include  -fopenmp     -I. -fPIC  -Wall -g -O2 -c Compute.cpp -o Compute.o
clang: error: unsupported option '-fopenmp'
make: *** [Compute.o] Error 1
ERROR: compilation failed for package ‘OpenMx’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/OpenMx/new/OpenMx.Rcheck/OpenMx’

```
### CRAN

```
* installing *source* package ‘OpenMx’ ...
** package ‘OpenMx’ successfully unpacked and MD5 sums checked
NOTE: ./configure is not an autoconf generated script.
Change default C/C++ compiler and default compile flags by editing ~/.R/Makevars
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/roxygen2/old/Rcpp/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/OpenMx/RcppEigen/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/OpenMx/StanHeaders/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/OpenMx/BH/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/OpenMx/rpf/include" -I/usr/local/include  -fopenmp     -I. -fPIC  -Wall -g -O2 -c Compute.cpp -o Compute.o
clang: error: unsupported option '-fopenmp'
make: *** [Compute.o] Error 1
ERROR: compilation failed for package ‘OpenMx’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/OpenMx/old/OpenMx.Rcheck/OpenMx’

```
# optmatch

Version: 0.9-10

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘CBPS’
    ```

# originr

Version: 0.3.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# osmdata

Version: 0.0.8

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# osmplotr

Version: 0.3.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 33-37 (basic-maps.Rmd) 
    Error: processing vignette 'basic-maps.Rmd' failed with diagnostics:
    there is no package called 'devtools'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        doc   5.9Mb
    ```

# pkgdown

Version: 1.1.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘devtools’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# pkgmaker

Version: 0.27

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘magrittr’ ‘stringi’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘devtools’
    ```

# pmap

Version: 0.3.2

## In both

*   checking examples ... ERROR
    ```
    ...
      [3m[90m<chr>[39m[23m            [3m[90m<chr>[39m[23m              [3m[90m<int>[39m[23m
    [90m1[39m Event 1 (normal) Event 1 (normal)      10
    [90m2[39m Event 1 (normal) Event 10 (target)      9
    [90m3[39m Event 1 (normal) Event 2 (normal)       4
    [90m4[39m Event 1 (normal) Event 3 (normal)       9
    [90m5[39m Event 1 (normal) Event 4 (normal)       7
    [90m6[39m Event 1 (normal) Event 5 (normal)      13
    > #  # A tibble: 6 x 3
    > #    from             to                amount
    > #    <chr>            <chr>              <int>
    > #  1 Event 1 (normal) Event 1 (normal)       8
    > #  2 Event 1 (normal) Event 10 (target)     10
    > #  3 Event 1 (normal) Event 2 (normal)      12
    > #  4 Event 1 (normal) Event 3 (normal)       9
    > #  5 Event 1 (normal) Event 4 (normal)       7
    > #  6 Event 1 (normal) Event 5 (normal)      10
    > p <- create_pmap_graph(nodes, edges, target_types = c("target"))
    Error in add_edges_from_table(p, table = edges %>% select(-amount), from_col = "from",  : 
      unused argument (ndf_mapping = "name_without_space")
    Calls: create_pmap_graph
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════
      OK: 59 SKIPPED: 0 FAILED: 9
      1. Error: create_pmap() should handle simple graph (@test_create_pmap.R#14) 
      2. Error: create_pmap() should handle complex graph (@test_create_pmap.R#40) 
      3. Error: create_pmap_graph() (@test_create_pmap_graph.R#30) 
      4. Error: prune_edges() should be able prune nothing (@test_prune_edges.R#4) 
      5. Error: prune_edges() should be able prune half of the edges (@test_prune_edges.R#30) 
      6. Error: prune_edges() should be able prune all of the edges (@test_prune_edges.R#58) 
      7. Error: prune_nodes() should be able prune nothing (@test_prune_nodes.R#4) 
      8. Error: prune_nodes() should be able prune half of the nodes (@test_prune_nodes.R#33) 
      9. Error: prune_nodes() should be able prune all of the nodes (@test_prune_nodes.R#63) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking whether package ‘pmap’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'add_edges_from_table(p, ': unused argument (ndf_mapping = "name_without_space") at create_pmap_graph.R:110 
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/pmap/new/pmap.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    create_pmap_graph: possible error in add_edges_from_table(p, table =
      edges %>% select(-amount), from_col = "from", to_col = "to",
      ndf_mapping = "name_without_space"): unused argument (ndf_mapping =
      "name_without_space")
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/pmap/new/pmap.Rcheck/00_pkg_src/pmap/R/create_pmap_graph.R:110-116)
    ```

# pnn

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘rgenoud’ in package code.
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    plot.sigma: no visible global function definition for ‘plot’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/pnn/new/pnn.Rcheck/00_pkg_src/pnn/R/smooth.R:63)
    smooth: no visible global function definition for ‘genoud’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/pnn/new/pnn.Rcheck/00_pkg_src/pnn/R/smooth.R:38)
    Undefined global functions or variables:
      genoud plot
    Consider adding
      importFrom("graphics", "plot")
    to your NAMESPACE file.
    ```

# popEpi

Version: 0.4.5

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        R     2.1Mb
        doc   2.7Mb
    ```

# populationPDXdesign

Version: 1.0.3

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘devtools’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# prioritizr

Version: 4.0.2

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    [[8]]
    [1] 9
    
    [[9]]
    [1] 10
    
    [[10]]
    [1] 11
    
    > 
    > # we could also use the parallel processing options available through "plyr"
    > # to use more computation resources to complete the jobs (note that since
    > # these jobs are very quick to process this is actually slower).
    > cl <- parallel::makeCluster(2, "PSOCK")
    Warning in socketConnection("localhost", port = port, server = TRUE, blocking = TRUE,  :
      port 11421 cannot be opened
    Error in socketConnection("localhost", port = port, server = TRUE, blocking = TRUE,  : 
      cannot open the connection
    Calls: <Anonymous> ... makePSOCKcluster -> newPSOCKnode -> socketConnection
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘gurobi’ ‘Rsymphony’
    ```

# projections

Version: 0.3.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘distcrete’
      All declared Imports should be used.
    ```

# pRoloc

Version: 1.20.2

## In both

*   checking examples ... ERROR
    ```
    ...
    > ### Title: Class '"ClustDist"'
    > ### Aliases: ClustDist class:ClustDist ClustDist-class
    > ###   plot,ClustDist,MSnSet-method show,ClustDist-method
    > ### Keywords: classes
    > 
    > ### ** Examples
    > 
    >   showClass("ClustDist")
    Class "ClustDist" [package "pRoloc"]
    
    Slots:
                                                                            
    Name:           k       dist       term         id       nrow    clustsz
    Class:    numeric       list  character  character    numeric       list
                                
    Name:  components       fcol
    Class:     vector  character
    >   
    >   library('pRolocdata')
    Error in library("pRolocdata") : there is no package called ‘pRolocdata’
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      This is pRoloc version 1.20.2 
        Visit https://lgatto.github.io/pRoloc/ to get started.
      
      Warning messages:
      1: In fun(libname, pkgname) :
        mzR has been built against a different Rcpp version (0.12.16)
      than is installed on your system (0.12.19). This might lead to errors
      when loading mzR. If you encounter such issues, please send a report,
      including the output of sessionInfo() to the Bioc support forum at 
      https://support.bioconductor.org/. For details see also
      https://github.com/sneumann/mzR/wiki/mzR-Rcpp-compiler-linker-issue.
      2: replacing previous import 'BiocGenerics::var' by 'stats::var' when loading 'MLInterfaces' 
      > library("pRolocdata")
      Error in library("pRolocdata") : there is no package called 'pRolocdata'
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following object is masked from 'package:tools':
    
        toHTML
    
    
    Attaching package: 'annotate'
    
    The following object is masked from 'package:mzR':
    
        nChrom
    
    Loading required package: cluster
    Warning: replacing previous import 'BiocGenerics::var' by 'stats::var' when loading 'MLInterfaces'
    
    This is pRoloc version 1.20.2 
      Visit https://lgatto.github.io/pRoloc/ to get started.
    
    Quitting from lines 87-93 (pRoloc-goannotations.Rmd) 
    Error: processing vignette 'pRoloc-goannotations.Rmd' failed with diagnostics:
    there is no package called 'pRolocdata'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘pRolocdata’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 15.0Mb
      sub-directories of 1Mb or more:
        R     3.0Mb
        doc  10.6Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘MLInterfaces:::.macroF1’ ‘MLInterfaces:::.precision’
      ‘MLInterfaces:::.recall’ ‘MLInterfaces:::es2df’
      ‘caret:::predict.plsda’
      See the note in ?`:::` about the use of this operator.
    There are ::: calls to the package's namespace in its code. A package
      almost never needs to use ::: for its own objects:
      ‘opt’
    ```

*   checking R code for possible problems ... NOTE
    ```
    Found the following possibly unsafe calls:
    File ‘pRoloc/R/annotation.R’:
      unlockBinding("params", .pRolocEnv)
    ```

# protoclass

Version: 1.0

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘class’ which was already attached by Depends.
      Please remove these calls from your code.
    Package in Depends field not imported from: ‘class’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    plot.protoclass: no visible global function definition for ‘box’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/protoclass/new/protoclass.Rcheck/00_pkg_src/protoclass/R/protoclass.r:344)
    plot.protoclass: no visible global function definition for ‘rgb2hsv’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/protoclass/new/protoclass.Rcheck/00_pkg_src/protoclass/R/protoclass.r:359)
    plot.protoclass: no visible global function definition for ‘col2rgb’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/protoclass/new/protoclass.Rcheck/00_pkg_src/protoclass/R/protoclass.r:359)
    plot.protoclass: no visible global function definition for ‘points’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/protoclass/new/protoclass.Rcheck/00_pkg_src/protoclass/R/protoclass.r:364-367)
    plot.protoclass: no visible global function definition for ‘hsv’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/protoclass/new/protoclass.Rcheck/00_pkg_src/protoclass/R/protoclass.r:364-367)
    plot.protoclass: no visible global function definition for ‘points’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/protoclass/new/protoclass.Rcheck/00_pkg_src/protoclass/R/protoclass.r:368-371)
    predict.protoclass: no visible global function definition for ‘knn1’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/protoclass/new/protoclass.Rcheck/00_pkg_src/protoclass/R/protoclass.r:150)
    Undefined global functions or variables:
      box col2rgb contour hsv knn1 lines par plot points rgb2hsv
    Consider adding
      importFrom("grDevices", "col2rgb", "hsv", "rgb2hsv")
      importFrom("graphics", "box", "contour", "lines", "par", "plot",
                 "points")
    to your NAMESPACE file.
    ```

# qsort

Version: 0.2.3

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6 marked UTF-8 strings
    ```

# qtlcharts

Version: 0.9-6

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# Quandl

Version: 2.9.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘xts’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# questionr

Version: 0.6.3

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘Hmisc’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4145 marked UTF-8 strings
    ```

# radiomics

Version: 0.1.3

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# randomsearch

Version: 0.1.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mlrMBO’
    ```

# raptr

Version: 0.1.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘gurobi’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        data   3.6Mb
    ```

# rAvis

Version: 0.1.4

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rAvis/new/rAvis.Rcheck/00_pkg_src/rAvis/R/plotFunctions.R:112)
    .avisRenderMapAdmin: no visible global function definition for ‘rect’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rAvis/new/rAvis.Rcheck/00_pkg_src/rAvis/R/plotFunctions.R:115-116)
    .avisRenderMapAdmin: no visible global function definition for ‘points’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rAvis/new/rAvis.Rcheck/00_pkg_src/rAvis/R/plotFunctions.R:117)
    .avisRenderMapPhysical: no visible global function definition for
      ‘points’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rAvis/new/rAvis.Rcheck/00_pkg_src/rAvis/R/plotFunctions.R:96)
    .avisRenderMapPhysical: no visible global function definition for
      ‘points’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rAvis/new/rAvis.Rcheck/00_pkg_src/rAvis/R/plotFunctions.R:99)
    avisMap: no visible global function definition for ‘par’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rAvis/new/rAvis.Rcheck/00_pkg_src/rAvis/R/plotFunctions.R:79)
    avisMap: no visible global function definition for ‘layout’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rAvis/new/rAvis.Rcheck/00_pkg_src/rAvis/R/plotFunctions.R:80)
    Undefined global functions or variables:
      layout par points read.csv rect
    Consider adding
      importFrom("graphics", "layout", "par", "points", "rect")
      importFrom("utils", "read.csv")
    to your NAMESPACE file.
    ```

# rbokeh

Version: 0.5.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘shiny’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# rbundler

Version: 0.3.7

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘devtools’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# rClinicalCodes

Version: 1.0.1

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    get_ClinicalCodes: no visible global function definition for ‘read.csv’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rClinicalCodes/new/rClinicalCodes.Rcheck/00_pkg_src/rClinicalCodes/R/clinicalcodes.R:28)
    get_ClinicalCodes: no visible global function definition for ‘read.csv’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rClinicalCodes/new/rClinicalCodes.Rcheck/00_pkg_src/rClinicalCodes/R/clinicalcodes.R:32)
    get_ClinicalCodes : <anonymous>: no visible global function definition
      for ‘read.csv’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rClinicalCodes/new/rClinicalCodes.Rcheck/00_pkg_src/rClinicalCodes/R/clinicalcodes.R:41)
    Undefined global functions or variables:
      read.csv
    Consider adding
      importFrom("utils", "read.csv")
    to your NAMESPACE file.
    ```

# RcppProgress

Version: 0.4.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# RCzechia

Version: 1.3.1

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Linking to GEOS 3.6.1, GDAL 2.1.3, PROJ 4.9.3
    RCzechia: downloading remote dataset.
    RCzechia: downloading remote dataset.
    Quitting from lines 104-127 (vignette.Rmd) 
    Error: processing vignette 'vignette.Rmd' failed with diagnostics:
    error reading from connection
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# rddapp

Version: 1.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# recexcavAAR

Version: 0.3.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 27-33 (recexcavAAR-vignette-1.Rmd) 
    Error: processing vignette 'recexcavAAR-vignette-1.Rmd' failed with diagnostics:
    there is no package called 'devtools'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# redland

Version: 1.0.17-10

## In both

*   checking whether package ‘redland’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/redland/new/redland.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘redland’ ...
** package ‘redland’ successfully unpacked and MD5 sums checked
Package redland was not found in the pkg-config search path.
Perhaps you should add the directory containing `redland.pc'
to the PKG_CONFIG_PATH environment variable
No package 'redland' found
Package redland was not found in the pkg-config search path.
Perhaps you should add the directory containing `redland.pc'
to the PKG_CONFIG_PATH environment variable
No package 'redland' found
Package redland was not found in the pkg-config search path.
Perhaps you should add the directory containing `redland.pc'
to the PKG_CONFIG_PATH environment variable
No package 'redland' found
Using PKG_CFLAGS=
Using PKG_LIBS=
------------------------- ANTICONF ERROR ---------------------------
Configuration failed because redland was not found. Try installing:
 * deb: librdf0-dev (Debian, Ubuntu, etc)
 * rpm: redland-devel (Fedora, EPEL)
 * brew: redland (OSX)
If redland is already installed, check that 'pkg-config' is in your
PATH and PKG_CONFIG_PATH contains a redland.pc file. If pkg-config
is unavailable you can set INCLUDE_DIR and LIB_DIR manually via:
R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
--------------------------------------------------------------------
ERROR: configuration failed for package ‘redland’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/redland/new/redland.Rcheck/redland’

```
### CRAN

```
* installing *source* package ‘redland’ ...
** package ‘redland’ successfully unpacked and MD5 sums checked
Package redland was not found in the pkg-config search path.
Perhaps you should add the directory containing `redland.pc'
to the PKG_CONFIG_PATH environment variable
No package 'redland' found
Package redland was not found in the pkg-config search path.
Perhaps you should add the directory containing `redland.pc'
to the PKG_CONFIG_PATH environment variable
No package 'redland' found
Package redland was not found in the pkg-config search path.
Perhaps you should add the directory containing `redland.pc'
to the PKG_CONFIG_PATH environment variable
No package 'redland' found
Using PKG_CFLAGS=
Using PKG_LIBS=
------------------------- ANTICONF ERROR ---------------------------
Configuration failed because redland was not found. Try installing:
 * deb: librdf0-dev (Debian, Ubuntu, etc)
 * rpm: redland-devel (Fedora, EPEL)
 * brew: redland (OSX)
If redland is already installed, check that 'pkg-config' is in your
PATH and PKG_CONFIG_PATH contains a redland.pc file. If pkg-config
is unavailable you can set INCLUDE_DIR and LIB_DIR manually via:
R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
--------------------------------------------------------------------
ERROR: configuration failed for package ‘redland’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/redland/old/redland.Rcheck/redland’

```
# refimpact

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘curl’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 85 marked UTF-8 strings
    ```

# rerddap

Version: 0.4.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘taxize’
    ```

# RGalaxy

Version: 1.24.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘RGalaxy-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: probeLookup
    > ### Title: Get the PFAM and SYMBOL names for a set of Affymetrix probe IDs
    > ### Aliases: probeLookup
    > 
    > ### ** Examples
    > 
    >   t <- tempfile()
    >   probeLookup("1002_f_at 1003_s_at", t)
    Failed with error:  ‘there is no package called ‘hgu95av2.db’’
    Error in loadNamespace(name) : there is no package called ‘hgu95av2.db’
    Calls: probeLookup ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Failed with error:  'there is no package called 'hgu95av2.db''
    Quitting from lines 349-350 (RGalaxy-vignette.Rmd) 
    Error: processing vignette 'RGalaxy-vignette.Rmd' failed with diagnostics:
    there is no package called 'hgu95av2.db'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘hgu95av2.db’
    
    Package which this enhances but not available for checking: ‘RSclient’
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘RSclient’ in package code.
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    Package in Depends field not imported from: ‘optparse’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    Unexported objects imported by ':::' calls:
      ‘tools:::.Rd_get_argument_table’ ‘tools:::.Rd_get_metadata’
      ‘tools:::fetchRdDB’
      See the note in ?`:::` about the use of this operator.
    ```

# Rilostat

Version: 0.2.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# riskyr

Version: 0.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# RItools

Version: 0.1-16

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘optmatch’
    ```

# rMouse

Version: 0.1

## In both

*   checking whether package ‘rMouse’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rMouse/new/rMouse.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rMouse’ ...
** package ‘rMouse’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded
Error: package or namespace load failed for ‘rMouse’:
 .onLoad failed in loadNamespace() for 'rMouse', details:
  call: NULL
  error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/rMouse/rJava/libs/rJava.so':
  dlopen(/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/rMouse/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/rMouse/rJava/libs/rJava.so
  Reason: image not found
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rMouse/new/rMouse.Rcheck/rMouse’

```
### CRAN

```
* installing *source* package ‘rMouse’ ...
** package ‘rMouse’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded
Error: package or namespace load failed for ‘rMouse’:
 .onLoad failed in loadNamespace() for 'rMouse', details:
  call: NULL
  error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/rMouse/rJava/libs/rJava.so':
  dlopen(/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/rMouse/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/rMouse/rJava/libs/rJava.so
  Reason: image not found
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rMouse/old/rMouse.Rcheck/rMouse’

```
# rpcdsearch

Version: 1.0

## In both

*   checking whether package ‘rpcdsearch’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rpcdsearch/new/rpcdsearch.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rpcdsearch’ ...
** package ‘rpcdsearch’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/rpcdsearch/rJava/libs/rJava.so':
  dlopen(/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/rpcdsearch/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/rpcdsearch/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘rpcdsearch’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rpcdsearch/new/rpcdsearch.Rcheck/rpcdsearch’

```
### CRAN

```
* installing *source* package ‘rpcdsearch’ ...
** package ‘rpcdsearch’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/rpcdsearch/rJava/libs/rJava.so':
  dlopen(/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/rpcdsearch/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/rpcdsearch/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘rpcdsearch’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rpcdsearch/old/rpcdsearch.Rcheck/rpcdsearch’

```
# rpf

Version: 0.59

## In both

*   checking whether package ‘rpf’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rpf/new/rpf.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rpf’ ...
** package ‘rpf’ successfully unpacked and MD5 sums checked
** libs
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/rpf/RcppEigen/include" -I/usr/local/include  -fopenmp    -fPIC  -Wall -g -O2  -c ba81quad.cpp -o ba81quad.o
clang: error: unsupported option '-fopenmp'
make: *** [ba81quad.o] Error 1
ERROR: compilation failed for package ‘rpf’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rpf/new/rpf.Rcheck/rpf’

```
### CRAN

```
* installing *source* package ‘rpf’ ...
** package ‘rpf’ successfully unpacked and MD5 sums checked
** libs
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/rpf/RcppEigen/include" -I/usr/local/include  -fopenmp    -fPIC  -Wall -g -O2  -c ba81quad.cpp -o ba81quad.o
clang: error: unsupported option '-fopenmp'
make: *** [ba81quad.o] Error 1
ERROR: compilation failed for package ‘rpf’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rpf/old/rpf.Rcheck/rpf’

```
# rstanarm

Version: 2.18.1

## In both

*   R CMD check timed out
    

# rstantools

Version: 1.5.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# RSuite

Version: 0.33-246

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘devtools’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# scriptexec

Version: 0.3.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# seaaroundus

Version: 1.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rgdal’
      All declared Imports should be used.
    ```

# SimRVPedigree

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# solrium

Version: 1.0.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘XML’
    ```

# SpidermiR

Version: 1.10.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘miRNAtap.db’
    
    Package suggested but not available for checking: ‘devtools’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# sqlutils

Version: 1.2

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘RPostgreSQL’ ‘RODBC’ ‘RMySQL’ ‘RJDBC’
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      ‘RJDBC’ ‘RMySQL’ ‘RODBC’ ‘RPostgreSQL’ ‘RSQLite’ ‘tcltk’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    cacheQuery: no visible global function definition for ‘read.csv’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/sqlutils/new/sqlutils.Rcheck/00_pkg_src/sqlutils/R/cacheQuery.r:28)
    cacheQuery: no visible global function definition for ‘write.csv’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/sqlutils/new/sqlutils.Rcheck/00_pkg_src/sqlutils/R/cacheQuery.r:39)
    Undefined global functions or variables:
      read.csv write.csv
    Consider adding
      importFrom("utils", "read.csv", "write.csv")
    to your NAMESPACE file.
    ```

# StarBioTrek

Version: 1.6.0

## In both

*   checking whether package ‘StarBioTrek’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/StarBioTrek/new/StarBioTrek.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

## Installation

### Devel

```
* installing *source* package ‘StarBioTrek’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘miRNAtap.db’
ERROR: lazy loading failed for package ‘StarBioTrek’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/StarBioTrek/new/StarBioTrek.Rcheck/StarBioTrek’

```
### CRAN

```
* installing *source* package ‘StarBioTrek’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘miRNAtap.db’
ERROR: lazy loading failed for package ‘StarBioTrek’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/StarBioTrek/old/StarBioTrek.Rcheck/StarBioTrek’

```
# STATegRa

Version: 1.16.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        data   2.4Mb
        doc    2.9Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    plotVAF,caClass: no visible binding for global variable ‘VAF’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/STATegRa/new/STATegRa.Rcheck/00_pkg_src/STATegRa/R/STATegRa_omicsPCA_plotting.R:118-125)
    plotVAF,caClass: no visible binding for global variable ‘block’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/STATegRa/new/STATegRa.Rcheck/00_pkg_src/STATegRa/R/STATegRa_omicsPCA_plotting.R:118-125)
    selectCommonComps,list-numeric: no visible binding for global variable
      ‘comps’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/STATegRa/new/STATegRa.Rcheck/00_pkg_src/STATegRa/R/STATegRa_omicsPCA_methods.R:1024-1027)
    selectCommonComps,list-numeric: no visible binding for global variable
      ‘block’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/STATegRa/new/STATegRa.Rcheck/00_pkg_src/STATegRa/R/STATegRa_omicsPCA_methods.R:1024-1027)
    selectCommonComps,list-numeric: no visible binding for global variable
      ‘comp’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/STATegRa/new/STATegRa.Rcheck/00_pkg_src/STATegRa/R/STATegRa_omicsPCA_methods.R:1041-1046)
    selectCommonComps,list-numeric: no visible binding for global variable
      ‘ratio’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/STATegRa/new/STATegRa.Rcheck/00_pkg_src/STATegRa/R/STATegRa_omicsPCA_methods.R:1041-1046)
    selectCommonComps,list-numeric: no visible binding for global variable
      ‘block’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/STATegRa/new/STATegRa.Rcheck/00_pkg_src/STATegRa/R/STATegRa_omicsPCA_methods.R:1041-1046)
    Undefined global functions or variables:
      VAF block comp components comps mylabel ratio
    ```

# SurfaceTortoise

Version: 0.1.0

## In both

*   checking whether package ‘SurfaceTortoise’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/SurfaceTortoise/new/SurfaceTortoise.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SurfaceTortoise’ ...
** package ‘SurfaceTortoise’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘xts’
ERROR: lazy loading failed for package ‘SurfaceTortoise’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/SurfaceTortoise/new/SurfaceTortoise.Rcheck/SurfaceTortoise’

```
### CRAN

```
* installing *source* package ‘SurfaceTortoise’ ...
** package ‘SurfaceTortoise’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘xts’
ERROR: lazy loading failed for package ‘SurfaceTortoise’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/SurfaceTortoise/old/SurfaceTortoise.Rcheck/SurfaceTortoise’

```
# taxa

Version: 0.3.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        R      2.0Mb
        data   1.1Mb
        doc    1.7Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘lazyeval’ ‘rlang’ ‘tidyr’
      All declared Imports should be used.
    ```

# TCGAbiolinksGUI

Version: 1.6.1

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available:
      ‘IlluminaHumanMethylation450kanno.ilmn12.hg19’
      ‘IlluminaHumanMethylation450kmanifest’
      ‘IlluminaHumanMethylation27kmanifest’
      ‘IlluminaHumanMethylation27kanno.ilmn12.hg19’
      ‘IlluminaHumanMethylationEPICanno.ilm10b2.hg19’
      ‘IlluminaHumanMethylationEPICmanifest’ ‘ELMER’
    
    Package suggested but not available for checking: ‘devtools’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# tcpl

Version: 1.4.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.7Mb
      sub-directories of 1Mb or more:
        R     1.0Mb
        sql   8.6Mb
    ```

# tcR

Version: 2.2.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        R      2.0Mb
        data   1.4Mb
        doc    3.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scales’
      All declared Imports should be used.
    ```

# testthis

Version: 1.0.4

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘devtools’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# textfeatures

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# tidyLPA

Version: 0.2.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

# TimeProjection

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      ‘ggplot2’ ‘plyr’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    Packages in Depends field not imported from:
      ‘Matrix’ ‘lubridate’ ‘timeDate’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/TimeProjection/new/TimeProjection.Rcheck/00_pkg_src/TimeProjection/R/calendarHeatmap.R:22-24)
    plotCalendarHeatmap: no visible binding for global variable ‘monthweek’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/TimeProjection/new/TimeProjection.Rcheck/00_pkg_src/TimeProjection/R/calendarHeatmap.R:22-24)
    plotCalendarHeatmap: no visible binding for global variable ‘weekday’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/TimeProjection/new/TimeProjection.Rcheck/00_pkg_src/TimeProjection/R/calendarHeatmap.R:22-24)
    plotCalendarHeatmap: no visible global function definition for
      ‘geom_tile’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/TimeProjection/new/TimeProjection.Rcheck/00_pkg_src/TimeProjection/R/calendarHeatmap.R:22-24)
    plotCalendarHeatmap: no visible global function definition for
      ‘facet_grid’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/TimeProjection/new/TimeProjection.Rcheck/00_pkg_src/TimeProjection/R/calendarHeatmap.R:22-24)
    plotCalendarHeatmap: no visible global function definition for
      ‘scale_fill_gradientn’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/TimeProjection/new/TimeProjection.Rcheck/00_pkg_src/TimeProjection/R/calendarHeatmap.R:22-24)
    projectDate: no visible global function definition for ‘holidayNYSE’
    projectDate: no visible global function definition for
      ‘sparse.model.matrix’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/TimeProjection/new/TimeProjection.Rcheck/00_pkg_src/TimeProjection/R/projection.R:75)
    Undefined global functions or variables:
      . aes ddply facet_grid geom_tile ggplot holidayNYSE isWeekday month
      monthweek scale_fill_gradientn sparse.model.matrix week weekday year
    ```

# uavRmp

Version: 0.5.4

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘devtools’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# vdiffr

Version: 0.2.3

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘devtools’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# vortexR

Version: 1.1.5

## In both

*   checking whether package ‘vortexR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/vortexR/new/vortexR.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

## Installation

### Devel

```
* installing *source* package ‘vortexR’ ...
** package ‘vortexR’ successfully unpacked and MD5 sums checked
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/vortexR/rJava/libs/rJava.so':
  dlopen(/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/vortexR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/vortexR/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘vortexR’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/vortexR/new/vortexR.Rcheck/vortexR’

```
### CRAN

```
* installing *source* package ‘vortexR’ ...
** package ‘vortexR’ successfully unpacked and MD5 sums checked
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/vortexR/rJava/libs/rJava.so':
  dlopen(/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/vortexR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/vortexR/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘vortexR’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/vortexR/old/vortexR.Rcheck/vortexR’

```
# xoi

Version: 0.67-4

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘devtools’
    ```

