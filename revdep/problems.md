# annotatr

Version: 1.2.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      mean(s[["diff_meth_mean"]]) not equal to 2.424537.
      1/1 mismatches
      [1] NA - 2.42 == NA
      
      [31m──[39m [31m2. Failure: Test summarize_numerical() and summarize_categorical() over small data (@test_6_summarize.R[39m
      sn1[["diff_meth_mean"]][which(sn1[["annot.id"]] == "inter:8599")] not equal to -1.0066888.
      target is NULL, current is numeric
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════════════════
      OK: 71 SKIPPED: 0 FAILED: 2
      1. Failure: Test summarize_numerical() (@test_6_summarize.R#58) 
      2. Failure: Test summarize_numerical() and summarize_categorical() over small data (@test_6_summarize.R#80) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   R CMD check timed out
    

*   checking R code for possible problems ... NOTE
    ```
    plot_coannotations: no visible binding for global variable ‘.’
    plot_numerical_coannotations: no visible binding for global variable
      ‘.’
    Undefined global functions or variables:
      .
    ```

# anomalize

Version: 0.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        help   4.7Mb
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

Version: 1.4.0

## In both

*   checking whether package ‘bacon’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘ggplot2’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/bacon/new/bacon.Rcheck/00install.out’ for details.
    ```

*   checking R code for possible problems ... NOTE
    ```
    .hist: no visible binding for global variable ‘..density..’
    .qq: no visible binding for global variable ‘column’
    Undefined global functions or variables:
      ..density.. column
    ```

# binomen

Version: 0.1.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘taxize’
    ```

# CDECRetrieve

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘lazyeval’ ‘purrr’ ‘roxygen2’
      All declared Imports should be used.
    ```

# chimeraviz

Version: 1.0.4

## In both

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘Biostrings’ ‘GenomicRanges’ ‘IRanges’ ‘Gviz’ ‘S4Vectors’ ‘ensembldb’
      ‘AnnotationFilter’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc       2.7Mb
        extdata   2.0Mb
    ```

# chipenrich.data

Version: 2.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 161.2Mb
      sub-directories of 1Mb or more:
        data  160.4Mb
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
    Undefined global functions or variables:
      na.omit
    Consider adding
      importFrom("stats", "na.omit")
    to your NAMESPACE file.
    ```

# clustermq

Version: 0.8.4.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘R6’ ‘infuser’ ‘purrr’
      All declared Imports should be used.
    ```

# codebook

Version: 0.5.9

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘graphics’ ‘htmltools’ ‘pander’ ‘readr’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘labelled’
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
      8: FUN(X[[i]], ...)
      9: lint(file, ..., parse_settings = FALSE)
      10: get_source_expressions(filename)
      11: extract_r_source(source_file$filename, source_file$lines)
      12: grep(pattern$chunk.begin, lines, perl = TRUE)
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════════════════
      OK: 18 SKIPPED: 0 FAILED: 1
      1. Error: check that package has google style (@test_code_style.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# CountClust

Version: 1.3.0

## In both

*   checking whether package ‘CountClust’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/CountClust/new/CountClust.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘CountClust’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** preparing package for lazy loading
Warning: package ‘ggplot2’ was built under R version 3.4.4
Error : object ‘switch_axis_position’ is not exported by 'namespace:cowplot'
ERROR: lazy loading failed for package ‘CountClust’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/CountClust/new/CountClust.Rcheck/CountClust’

```
### CRAN

```
* installing *source* package ‘CountClust’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** preparing package for lazy loading
Warning: package ‘ggplot2’ was built under R version 3.4.4
Error : object ‘switch_axis_position’ is not exported by 'namespace:cowplot'
ERROR: lazy loading failed for package ‘CountClust’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/CountClust/old/CountClust.Rcheck/CountClust’

```
# curatedMetagenomicData

Version: 1.2.2

## In both

*   checking whether package ‘curatedMetagenomicData’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘dplyr’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/curatedMetagenomicData/new/curatedMetagenomicData.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
     / __| | | | '__/ _` | __/ _ \/ _` |
    | (__| |_| | | | (_| | ||  __/ (_| |
     \___|\__,_|_|  \__,_|\__\___|\__,_|
     __  __      _                                         _
    |  \/  | ___| |_ __ _  __ _  ___ _ __   ___  _ __ ___ (_) ___
    | |\/| |/ _ \ __/ _` |/ _` |/ _ \ '_ \ / _ \| '_ ` _ \| |/ __|
    | |  | |  __/ || (_| | (_| |  __/ | | | (_) | | | | | | | (__
    |_|  |_|\___|\__\__,_|\__, |\___|_| |_|\___/|_| |_| |_|_|\___|
     ____        _        |___/
    |  _ \  __ _| |_ __ _
    | | | |/ _` | __/ _` |
    | |_| | (_| | || (_| |
    |____/ \__,_|\__\__,_|
    
    Consider using the development version of curatedMetagenomicData,
    as the database has expanded considerably since the last release.
    See tinyurl.com/datasets-included for further information.
    Quitting from lines 325-327 (curatedMetagenomicData.Rmd) 
    Error: processing vignette 'curatedMetagenomicData.Rmd' failed with diagnostics:
    "veg_distance" not available for .C() for package "vegan"
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘dplyr’ ‘phyloseq’ ‘Biobase’ ‘ExperimentHub’ ‘AnnotationHub’
      ‘magrittr’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.5Mb
      sub-directories of 1Mb or more:
        help   7.9Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘BiocInstaller’
    A package should be listed in only one of these fields.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘BiocInstaller’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ExpressionSet2MRexperiment: no visible global function definition for
      ‘AnnotatedDataFrame’
    ExpressionSet2MRexperiment: no visible global function definition for
      ‘phenoData’
    curatedMetagenomicData : <anonymous>: no visible global function
      definition for ‘exprs<-’
    Undefined global functions or variables:
      AnnotatedDataFrame exprs<- phenoData
    ```

*   checking Rd files ... NOTE
    ```
    prepare_Rd: HMP_2012.Rd:540-542: Dropping empty section \seealso
    prepare_Rd: KarlssonFH_2013.Rd:90-92: Dropping empty section \seealso
    prepare_Rd: LeChatelierE_2013.Rd:86-88: Dropping empty section \seealso
    prepare_Rd: LomanNJ_2013_Hi.Rd:82-84: Dropping empty section \seealso
    prepare_Rd: LomanNJ_2013_Mi.Rd:82-84: Dropping empty section \seealso
    prepare_Rd: NielsenHB_2014.Rd:94-96: Dropping empty section \seealso
    prepare_Rd: Obregon_TitoAJ_2015.Rd:94-96: Dropping empty section \seealso
    prepare_Rd: OhJ_2014.Rd:86-88: Dropping empty section \seealso
    prepare_Rd: QinJ_2012.Rd:106-108: Dropping empty section \seealso
    prepare_Rd: QinN_2014.Rd:94-96: Dropping empty section \seealso
    prepare_Rd: RampelliS_2015.Rd:90-92: Dropping empty section \seealso
    prepare_Rd: TettAJ_2016.Rd:184-186: Dropping empty section \seealso
    prepare_Rd: ZellerG_2014.Rd:94-96: Dropping empty section \seealso
    ```

# datadr

Version: 0.8.6

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Rhipe’
    ```

# deisotoper

Version: 0.0.3

## In both

*   checking whether package ‘deisotoper’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘rJava’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/deisotoper/new/deisotoper.Rcheck/00install.out’ for details.
    ```

# df2json

Version: 0.0.2

## In both

*   checking whether package ‘df2json’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘rjson’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/df2json/new/df2json.Rcheck/00install.out’ for details.
    ```

# dodgr

Version: 0.0.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    ```

# dotCall64

Version: 0.9-5.2

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
ccache clang -Qunused-arguments  -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG   -I/usr/local/include  -fopenmp -I../inst/include/ -DDOTCAL64_PRIVATE -fPIC  -Wall -g -O2  -c dotCall64.c -o dotCall64.o
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
ccache clang -Qunused-arguments  -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG   -I/usr/local/include  -fopenmp -I../inst/include/ -DDOTCAL64_PRIVATE -fPIC  -Wall -g -O2  -c dotCall64.c -o dotCall64.o
clang: error: unsupported option '-fopenmp'
make: *** [dotCall64.o] Error 1
ERROR: compilation failed for package ‘dotCall64’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/dotCall64/old/dotCall64.Rcheck/dotCall64’

```
# dynr

Version: 0.1.12-5

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
# epivizr

Version: 2.6.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    10: timing_fn(handle(ev <- withCallingHandlers(withVisible(eval(expr,     envir, enclos)), warning = wHandler, error = eHandler, message = mHandler)))
    11: evaluate_call(expr, parsed$src[[i]], envir = envir, enclos = enclos,     debug = debug, last = i == length(out), use_try = stop_on_error !=         2L, keep_warning = keep_warning, keep_message = keep_message,     output_handler = output_handler, include_timing = include_timing)
    12: evaluate::evaluate(...)
    13: evaluate(code, envir = env, new_device = FALSE, keep_warning = !isFALSE(options$warning),     keep_message = !isFALSE(options$message), stop_on_error = if (options$error &&         options$include) 0L else 2L, output_handler = knit_handlers(options$render,         options))
    14: in_dir(input_dir(), evaluate(code, envir = env, new_device = FALSE,     keep_warning = !isFALSE(options$warning), keep_message = !isFALSE(options$message),     stop_on_error = if (options$error && options$include) 0L else 2L,     output_handler = knit_handlers(options$render, options)))
    15: block_exec(params)
    16: call_block(x)
    17: process_group.block(group)
    18: process_group(group)
    19: withCallingHandlers(if (tangle) process_tangle(group) else process_group(group),     error = function(e) {        setwd(wd)        cat(res, sep = "\n", file = output %n% "")        message("Quitting from lines ", paste(current_lines(i),             collapse = "-"), " (", knit_concord$get("infile"),             ") ")    })
    20: process_file(text, output)
    21: knitr::knit(knit_input, knit_output, envir = envir, quiet = quiet,     encoding = encoding)
    22: rmarkdown::render(file, encoding = encoding, quiet = quiet, envir = globalenv())
    23: vweave_rmarkdown(...)
    24: engine$weave(file, quiet = quiet, encoding = enc)
    25: doTryCatch(return(expr), name, parentenv, handler)
    26: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    27: tryCatchList(expr, classes, parentenv, handlers)
    28: tryCatch({    engine$weave(file, quiet = quiet, encoding = enc)    setwd(startdir)    find_vignette_product(name, by = "weave", engine = engine)}, error = function(e) {    stop(gettextf("processing vignette '%s' failed with diagnostics:\n%s",         file, conditionMessage(e)), domain = NA, call. = FALSE)})
    29: buildVignettes(dir = "/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/epivizr/new/epivizr.Rcheck/vign_test/epivizr")
    An irrecoverable exception occurred. R is aborting now ...
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Mus.musculus’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘minfi’
    ```

# epivizrData

Version: 1.4.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Mus.musculus’
    ```

# eurostat

Version: 3.2.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 484 marked UTF-8 strings
    ```

# fakemake

Version: 1.3.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rcmdcheck’
    ```

# flippant

Version: 1.1.0

## In both

*   checking whether package ‘flippant’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘ggplot2’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/flippant/new/flippant.Rcheck/00install.out’ for details.
    ```

# forecastHybrid

Version: 3.0.14

## In both

*   checking whether package ‘forecastHybrid’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘forecast’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/forecastHybrid/new/forecastHybrid.Rcheck/00install.out’ for details.
    ```

# gapfill

Version: 0.9.6

## In both

*   checking whether package ‘gapfill’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘ggplot2’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/gapfill/new/gapfill.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘raster’ ‘doParallel’ ‘doMPI’
    ```

# gbfs

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘roxygen2’ ‘testthat’ ‘utils’ ‘withr’
      All declared Imports should be used.
    ```

# GenVisR

Version: 1.6.3

## In both

*   checking R code for possible problems ... NOTE
    ```
    waterfall: warning in waterfall_align(genes = gene_plot, heatmap =
      heatmap, burden = burden_plot, clinical = clinical_plot, proportion =
      proportions_plot, section_heights = section_heights): partial
      argument match of 'proportion' to 'proportions'
    waterfall: warning in waterfall_align(genes = gene_plot, heatmap =
      heatmap, burden = burden_plot, proportion = proportions_plot,
      section_heights = section_heights): partial argument match of
      'proportion' to 'proportions'
    ```

# geonapi

Version: 0.1-0

## In both

*   checking whether package ‘geonapi’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘geometa’ was built under R version 3.4.3
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/geonapi/new/geonapi.Rcheck/00install.out’ for details.
    ```

# GGally

Version: 1.4.0

## In both

*   checking whether package ‘GGally’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘ggplot2’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/GGally/new/GGally.Rcheck/00install.out’ for details.
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

# googleAuthR

Version: 0.6.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
      All declared Imports should be used.
    ```

# gppm

Version: 0.2.0

## In both

*   checking whether package ‘gppm’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘Rcpp’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/gppm/new/gppm.Rcheck/00install.out’ for details.
    ```

# gqlr

Version: 0.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        R   6.1Mb
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

# icd

Version: 3.2.0

## In both

*   checking whether package ‘icd’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘icd.data’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/icd/new/icd.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 24.1Mb
      sub-directories of 1Mb or more:
        R     16.1Mb
        doc    5.8Mb
        libs   1.3Mb
    ```

# jqr

Version: 1.0.0

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
# JuniperKernel

Version: 1.4.1.0

## In both

*   checking whether package ‘JuniperKernel’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘pbdZMQ’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/JuniperKernel/new/JuniperKernel.Rcheck/00install.out’ for details.
    ```

# lawn

Version: 0.4.2

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘maps’ ‘geojsonio’
    ```

# Logolas

Version: 1.0.0

## In both

*   checking files in ‘vignettes’ ... NOTE
    ```
    The following directory looks like a leftover from 'knitr':
      ‘figure’
    Please remove from your package.
    ```

# mase

Version: 0.1.1

## In both

*   checking whether package ‘mase’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘glmnet’ was built under R version 3.4.4
      Warning: package ‘Matrix’ was built under R version 3.4.4
      Warning: package ‘foreach’ was built under R version 3.4.3
      Warning: package ‘rpms’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/mase/new/mase.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘MASS’
      All declared Imports should be used.
    ```

# MAST

Version: 1.2.1

## In both

*   checking whether package ‘MAST’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘matrixStats’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/MAST/new/MAST.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning: package 'ggplot2' was built under R version 3.4.4
    Warning: package 'GGally' was built under R version 3.4.4
    Warning: package 'XML' was built under R version 3.4.4
    Warning: package 'reshape2' was built under R version 3.4.3
    Warning: package 'data.table' was built under R version 3.4.4
    Warning: package 'knitr' was built under R version 3.4.3
    Warning: package 'NMF' was built under R version 3.4.4
    Warning: package 'pkgmaker' was built under R version 3.4.4
    Warning: package 'registry' was built under R version 3.4.3
    Warning: package 'rngtools' was built under R version 3.4.4
    Warning: package 'cluster' was built under R version 3.4.4
    Warning: package 'rsvd' was built under R version 3.4.3
    Warning: package 'matrixStats' was built under R version 3.4.4
    Quitting from lines 90-102 (MAITAnalysis.Rmd) 
    Error: processing vignette 'MAITAnalysis.Rmd' failed with diagnostics:
    Columns in 'columns' not found in data: c('PC1', 'PC2', 'PC3'). Choices: c('V1', 'V2', 'V3', 'V4', 'wellKey', 'condition', 'nGeneOn', 'libSize', 'PercentToHuman', 'MedianCVCoverage', 'PCRDuplicate', 'exonRate', 'pastFastqc', 'ncells', 'ngeneson', 'cngeneson', 'TRAV1', 'TRBV6', 'TRBV4', 'TRBV20', 'alpha', 'beta', 'ac', 'bc', 'ourfilter')
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        data   3.7Mb
        doc    1.9Mb
    ```

# mgarchBEKK

Version: 0.0.2

## In both

*   checking whether package ‘mgarchBEKK’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘tseries’ was built under R version 3.4.4
      Warning: package ‘mvtnorm’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/mgarchBEKK/new/mgarchBEKK.Rcheck/00install.out’ for details.
    ```

# miscFuncs

Version: 1.2-10

## In both

*   checking whether package ‘miscFuncs’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘mvtnorm’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/miscFuncs/new/miscFuncs.Rcheck/00install.out’ for details.
    ```

# mizer

Version: 1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      Warning message:
      package 'testthat' was built under R version 3.4.3 
      > library(mizer)
      > 
      > test_check("mizer")
      [31m──[39m [31m1. Failure: retune_abundance reproduces scaling model (@test-wrapper_functions.R#17) [39m [31m─────────────────[39m
      max(abs(initial_n - pr@initial_n)) is not strictly less than 2e-11. Difference: 2.47e-11
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════════════════
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

# MoonlightR

Version: 1.2.0

## In both

*   checking whether package ‘MoonlightR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘foreach’ was built under R version 3.4.3
      Warning: package ‘iterators’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/MoonlightR/new/MoonlightR.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        data   3.1Mb
        doc    2.6Mb
    ```

# mregions

Version: 0.1.6

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘leaflet’
    ```

# MSnbase

Version: 2.2.0

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

*   R CMD check timed out
    

*   checking whether package ‘MSnbase’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘Rcpp’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/MSnbase/new/MSnbase.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘pRolocdata’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.3Mb
      sub-directories of 1Mb or more:
        data   1.9Mb
        doc    4.3Mb
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

Version: 1.8.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    countsInBlankSamples_verticalPlot: no visible global function
      definition for ‘png’
    countsInBlankSamples_verticalPlot: no visible global function
      definition for ‘dev.off’
    geneClustering: no visible global function definition for ‘png’
    geneClustering: no visible global function definition for ‘dev.off’
    negCtrlsByLane_verticalPlot: no visible global function definition for
      ‘png’
    negCtrlsByLane_verticalPlot: no visible global function definition for
      ‘dev.off’
    previewPNG: no visible global function definition for ‘png’
    previewPNG: no visible global function definition for ‘dev.off’
    sampleClustering,RccSet: no visible global function definition for
      ‘png’
    sampleClustering,RccSet: no visible global function definition for
      ‘dev.off’
    Undefined global functions or variables:
      dev.off png
    Consider adding
      importFrom("grDevices", "dev.off", "png")
    to your NAMESPACE file.
    ```

# nima

Version: 0.5.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ProjectTemplate’ ‘devtools’ ‘plyr’ ‘survival’
      All declared Imports should be used.
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
    Pwr.lme: no visible global function definition for ‘anova’
    Pwr.lme: no visible global function definition for ‘qf’
    Pwr.lme: no visible global function definition for ‘pf’
    logLik1.lme: no visible global function definition for ‘coef’
    logLik1.lme: no visible global function definition for ‘model.matrix’
    logLik1.lme: no visible global function definition for ‘predict’
    logLik1.lme: no visible global function definition for ‘formula’
    runScript: no visible binding for global variable ‘example’
    sigmaTolme: no visible global function definition for ‘vcov’
    simulateY.lme: no visible global function definition for ‘runif’
    simulateY.lme: no visible global function definition for ‘fitted’
    simulateY.lme: no visible global function definition for ‘rnorm’
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

Version: 2.9.9

## In both

*   R CMD check timed out
    

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Rmpi’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 14.1Mb
      sub-directories of 1Mb or more:
        R        3.0Mb
        libs     3.6Mb
        models   4.7Mb
    ```

# optmatch

Version: 0.9-10

## In both

*   checking whether package ‘optmatch’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘survival’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/optmatch/new/optmatch.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘CBPS’
    ```

# originr

Version: 0.3.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘taxize’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# osmplotr

Version: 0.3.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        doc   5.9Mb
    ```

# paleobioDB

Version: 0.5.0

## In both

*   checking whether package ‘paleobioDB’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘maps’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/paleobioDB/new/paleobioDB.Rcheck/00install.out’ for details.
    ```

# pkgmaker

Version: 0.27

## In both

*   checking whether package ‘pkgmaker’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘registry’ was built under R version 3.4.3
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/pkgmaker/new/pkgmaker.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘magrittr’ ‘stringi’
      All declared Imports should be used.
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
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════════════════
      OK: 62 SKIPPED: 0 FAILED: 9
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

*   checking R code for possible problems ... NOTE
    ```
    create_pmap_graph: possible error in add_edges_from_table(p, table =
      edges %>% select(-amount), from_col = "from", to_col = "to",
      ndf_mapping = "name_without_space"): unused argument (ndf_mapping =
      "name_without_space")
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
    smooth: no visible global function definition for ‘genoud’
    Undefined global functions or variables:
      genoud plot
    Consider adding
      importFrom("graphics", "plot")
    to your NAMESPACE file.
    ```

# prioritizr

Version: 4.0.2

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘gurobi’ ‘Rsymphony’
    ```

# projections

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘distcrete’
      All declared Imports should be used.
    ```

# pRoloc

Version: 1.16.1

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
      Warning messages:
      1: package 'Rcpp' was built under R version 3.4.4 
      2: In fun(libname, pkgname) :
        mzR has been built against a different Rcpp version (0.12.10)
      than is installed on your system (0.12.18). This might lead to errors
      when loading mzR. If you encounter such issues, please send a report,
      including the output of sessionInfo() to the Bioc support forum at 
      https://support.bioconductor.org/. For details see also
      https://github.com/sneumann/mzR/wiki/mzR-Rcpp-compiler-linker-issue.
      3: package 'XML' was built under R version 3.4.4 
      4: package 'cluster' was built under R version 3.4.4 
      5: replacing previous import 'BiocGenerics::var' by 'stats::var' when loading 'MLInterfaces' 
      > library("pRolocdata")
      Error in library("pRolocdata") : there is no package called 'pRolocdata'
      Execution halted
    ```

*   checking whether package ‘pRoloc’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘Rcpp’ was built under R version 3.4.4
      Warning: package ‘XML’ was built under R version 3.4.4
      Warning: package ‘cluster’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/pRoloc/new/pRoloc.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        toHTML
    
    
    Attaching package: 'annotate'
    
    The following object is masked from 'package:mzR':
    
        nChrom
    
    Loading required package: cluster
    Warning: package 'cluster' was built under R version 3.4.4
    Warning: replacing previous import 'BiocGenerics::var' by 'stats::var' when loading 'MLInterfaces'
    
    This is pRoloc version 1.16.1 
      Read '?pRoloc' and references therein for information
      about the package and how to get started.
    
    Quitting from lines 80-86 (pRoloc-goannotations.Rmd) 
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
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc   3.3Mb
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
      ‘checkSortedFeatureNames’ ‘opt’
    ```

*   checking R code for possible problems ... NOTE
    ```
    Found the following possibly unsafe calls:
    File ‘pRoloc/R/annotation.R’:
      unlockBinding("params", .pRolocEnv)
    ```

*   checking files in ‘vignettes’ ... NOTE
    ```
    The following directory looks like a leftover from 'knitr':
      ‘figure’
    Please remove from your package.
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
    addcircle: no visible global function definition for ‘lines’
    addclassifierboundary: no visible global function definition for ‘par’
    addclassifierboundary: no visible global function definition for
      ‘contour’
    plot.protoclass: no visible global function definition for ‘plot’
    plot.protoclass: no visible global function definition for ‘box’
    plot.protoclass: no visible global function definition for ‘rgb2hsv’
    plot.protoclass: no visible global function definition for ‘col2rgb’
    plot.protoclass: no visible global function definition for ‘points’
    plot.protoclass: no visible global function definition for ‘hsv’
    predict.protoclass: no visible global function definition for ‘knn1’
    Undefined global functions or variables:
      box col2rgb contour hsv knn1 lines par plot points rgb2hsv
    Consider adding
      importFrom("grDevices", "col2rgb", "hsv", "rgb2hsv")
      importFrom("graphics", "box", "contour", "lines", "par", "plot",
                 "points")
    to your NAMESPACE file.
    ```

# qsort

Version: 0.2.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6 marked UTF-8 strings
    ```

# Quandl

Version: 2.8.0

## In both

*   checking whether package ‘Quandl’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘xts’ was built under R version 3.4.4
      Warning: package ‘zoo’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/Quandl/new/Quandl.Rcheck/00install.out’ for details.
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

# R4CouchDB

Version: 0.7.5

## In both

*   checking whether package ‘R4CouchDB’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘RCurl’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/R4CouchDB/new/R4CouchDB.Rcheck/00install.out’ for details.
    ```

# randomsearch

Version: 0.1.0

## In both

*   checking whether package ‘randomsearch’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘ParamHelpers’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/randomsearch/new/randomsearch.Rcheck/00install.out’ for details.
    ```

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
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        data   3.6Mb
    ```

# rAvis

Version: 0.1.4

## In both

*   checking R code for possible problems ... NOTE
    ```
    .avisApiBusAvanzada: no visible global function definition for
      ‘read.csv’
    .avisRenderMapAdmin: no visible global function definition for ‘points’
    .avisRenderMapAdmin: no visible global function definition for ‘rect’
    .avisRenderMapPhysical: no visible global function definition for
      ‘points’
    avisMap: no visible global function definition for ‘par’
    avisMap: no visible global function definition for ‘layout’
    Undefined global functions or variables:
      layout par points read.csv rect
    Consider adding
      importFrom("graphics", "layout", "par", "points", "rect")
      importFrom("utils", "read.csv")
    to your NAMESPACE file.
    ```

# rbison

Version: 0.6.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘taxize’
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

*   checking whether package ‘rbundler’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘devtools’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rbundler/new/rbundler.Rcheck/00install.out’ for details.
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    ```

*   checking R code for possible problems ... NOTE
    ```
    find_available_versions: no visible global function definition for
      ‘contrib.url’
    find_available_versions: no visible global function definition for
      ‘available.packages’
    install_version: no visible global function definition for
      ‘contrib.url’
    install_version: no visible global function definition for
      ‘available.packages’
    install_version: no visible global function definition for
      ‘install_url’
    load_available_packages: no visible global function definition for
      ‘contrib.url’
    validate_installed_package: no visible global function definition for
      ‘installed.packages’
    Undefined global functions or variables:
      available.packages contrib.url install_url installed.packages
    Consider adding
      importFrom("utils", "available.packages", "contrib.url",
                 "installed.packages")
    to your NAMESPACE file.
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
    get_ClinicalCodes : <anonymous>: no visible global function definition
      for ‘read.csv’
    Undefined global functions or variables:
      read.csv
    Consider adding
      importFrom("utils", "read.csv")
    to your NAMESPACE file.
    ```

# RcppProgress

Version: 0.4.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      [31m──[39m [31m4. Failure: eta_progress_bar (@test-pkg_examples.R#28) [39m [31m───────────────────────────────────────────────[39m
      `eta_progress_bar(nb = 1000)` threw an error.
      Message: Command failed (1)
      Class:   simpleError/error/condition
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════════════════
      OK: 0 SKIPPED: 0 FAILED: 4
      1. Failure: test_sequential (@test-pkg_examples.R#6) 
      2. Failure: test_multithreaded (@test-pkg_examples.R#13) 
      3. Failure: amardillo_multithreaded (@test-pkg_examples.R#20) 
      4. Failure: eta_progress_bar (@test-pkg_examples.R#28) 
      
      Error: testthat unit tests failed
      Execution halted
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

Version: 1.20.1

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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/runTests.R’ failed.
    Last 13 lines of output:
      FAILURE in test_missing_param: Error in checkEquals(expected, output) : 1 string mismatch
      
      
      Test files with failing tests
      
         test_galaxy.R 
           test_missing_param 
      
      
      Error in BiocGenerics:::testPackage("RGalaxy") : 
        unit tests failed for package RGalaxy
      In addition: Warning messages:
      1: Not enough information to create a functional test. 
      2: Not enough information to create a functional test. 
      Execution halted
    ```

*   checking whether package ‘RGalaxy’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘XML’ was built under R version 3.4.4
      Warning: package ‘optparse’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/RGalaxy/new/RGalaxy.Rcheck/00install.out’ for details.
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
    Packages in Depends field not imported from:
      ‘digest’ ‘optparse’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    Unexported objects imported by ':::' calls:
      ‘tools:::.Rd_get_argument_table’ ‘tools:::.Rd_get_metadata’
      ‘tools:::fetchRdDB’
      See the note in ?`:::` about the use of this operator.
    ```

# riskyr

Version: 0.1.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required and available but unsuitable versions: ‘grid’ ‘utils’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
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
** preparing package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded
Error: package or namespace load failed for ‘rMouse’:
 .onLoad failed in loadNamespace() for 'rMouse', details:
  call: rJava::.jnew("java/awt/Robot")
  error: java.awt.AWTException: headless environment
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
** preparing package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded
Error: package or namespace load failed for ‘rMouse’:
 .onLoad failed in loadNamespace() for 'rMouse', details:
  call: rJava::.jnew("java/awt/Robot")
  error: java.awt.AWTException: headless environment
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rMouse/old/rMouse.Rcheck/rMouse’

```
# rnoaa

Version: 0.7.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘taxize’
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
ccache clang++ -Qunused-arguments  -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/rpf/RcppEigen/include" -I/usr/local/include  -fopenmp    -fPIC  -Wall -g -O2  -c ba81quad.cpp -o ba81quad.o
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
ccache clang++ -Qunused-arguments  -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG  -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/rpf/RcppEigen/include" -I/usr/local/include  -fopenmp    -fPIC  -Wall -g -O2  -c ba81quad.cpp -o ba81quad.o
clang: error: unsupported option '-fopenmp'
make: *** [ba81quad.o] Error 1
ERROR: compilation failed for package ‘rpf’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rpf/old/rpf.Rcheck/rpf’

```
# RQEntangle

Version: 0.1.1

## In both

*   checking whether package ‘RQEntangle’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘iterators’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/RQEntangle/new/RQEntangle.Rcheck/00install.out’ for details.
    ```

# rstanarm

Version: 2.17.4

## In both

*   R CMD check timed out
    

# rstantools

Version: 1.5.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      20: eval(substitute(expr), data, enclos = parent.frame())
      21: with_envvar(env, run(bin, args = real_cmdargs, stdout_line_callback = real_callback(stdout), stderr_line_callback = real_callback(stderr), 
             stdout_callback = real_block_callback, stderr_callback = real_block_callback, echo_cmd = echo, echo = show, 
             spinner = spinner, error_on_status = fail_on_status, timeout = timeout))
      22: force(code)
      23: run(bin, args = real_cmdargs, stdout_line_callback = real_callback(stdout), stderr_line_callback = real_callback(stderr), 
             stdout_callback = real_block_callback, stderr_callback = real_block_callback, echo_cmd = echo, echo = show, 
             spinner = spinner, error_on_status = fail_on_status, timeout = timeout)
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════════════════
      OK: 41 SKIPPED: 0 FAILED: 1
      1. Error: (unknown) (@test-rstan_package_skeleton.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
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

Version: 0.1.0

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

Version: 1.7.4

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘miRNAtap.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# spocc

Version: 0.8.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Warning message:
      package 'testthat' was built under R version 3.4.3 
      > test_check("spocc")
      Loading required package: spocc
      [31m──[39m [31m1. Error: (unknown) (@test-taxize-integration.R#3) [39m [31m───────────────────────────────────────────────────[39m
      there is no package called 'taxize'
      1: library("taxize") at testthat/test-taxize-integration.R:3
      2: stop(txt, domain = NA)
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════════════════
      OK: 0 SKIPPED: 24 FAILED: 1
      1. Error: (unknown) (@test-taxize-integration.R#3) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘taxize’
    ```

# spotGUI

Version: 0.2.0

## In both

*   checking whether package ‘spotGUI’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘ParamHelpers’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/spotGUI/new/spotGUI.Rcheck/00install.out’ for details.
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
    cacheQuery: no visible global function definition for ‘write.csv’
    Undefined global functions or variables:
      read.csv write.csv
    Consider adding
      importFrom("utils", "read.csv", "write.csv")
    to your NAMESPACE file.
    ```

# StarBioTrek

Version: 1.2.1

## In both

*   checking whether package ‘StarBioTrek’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/StarBioTrek/new/StarBioTrek.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘StarBioTrek’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** preparing package for lazy loading
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
** preparing package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘miRNAtap.db’
ERROR: lazy loading failed for package ‘StarBioTrek’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/StarBioTrek/old/StarBioTrek.Rcheck/StarBioTrek’

```
# STATegRa

Version: 1.10.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   2.4Mb
        doc    2.4Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    biplotRes,caClass-character-numeric-character: no visible binding for
      global variable ‘values.1’
    biplotRes,caClass-character-numeric-character: no visible binding for
      global variable ‘values.2’
    biplotRes,caClass-character-numeric-character: no visible binding for
      global variable ‘color’
    plotVAF,caClass: no visible binding for global variable ‘comp’
    plotVAF,caClass: no visible binding for global variable ‘VAF’
    plotVAF,caClass: no visible binding for global variable ‘block’
    selectCommonComps,matrix-matrix-numeric: no visible binding for global
      variable ‘comps’
    selectCommonComps,matrix-matrix-numeric: no visible binding for global
      variable ‘block’
    selectCommonComps,matrix-matrix-numeric: no visible binding for global
      variable ‘comp’
    selectCommonComps,matrix-matrix-numeric: no visible binding for global
      variable ‘ratio’
    Undefined global functions or variables:
      VAF block color comp comps ratio values.1 values.2
    ```

# taxa

Version: 0.2.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘taxize’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# TCGAbiolinksGUI

Version: 1.2.1

## In both

*   checking whether package ‘TCGAbiolinksGUI’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘shinydashboard’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/TCGAbiolinksGUI/new/TCGAbiolinksGUI.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following objects are masked from 'package:S4Vectors':
    
        first, intersect, rename, setdiff, setequal, union
    
    The following objects are masked from 'package:BiocGenerics':
    
        combine, intersect, setdiff, union
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 11-15 (data.Rmd) 
    Error: processing vignette 'data.Rmd' failed with diagnostics:
    there is no package called 'DT'
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 30.3Mb
      sub-directories of 1Mb or more:
        app   1.0Mb
        doc  28.9Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    'library' or 'require' calls not declared from:
      ‘DT’ ‘dplyr’
    ```

# tcpl

Version: 1.4.3

## In both

*   checking whether package ‘tcpl’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘data.table’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/tcpl/new/tcpl.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.8Mb
      sub-directories of 1Mb or more:
        sql   8.6Mb
    ```

# tcR

Version: 2.2.1.11

## In both

*   checking whether package ‘tcR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘ggplot2’ was built under R version 3.4.4
      Warning: package ‘dplyr’ was built under R version 3.4.4
      Warning: package ‘reshape2’ was built under R version 3.4.3
      Warning: package ‘igraph’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/tcR/new/tcR.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   1.2Mb
        doc    3.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scales’
      All declared Imports should be used.
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'top.fun':
      ‘slice.fun’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# TimeProjection

Version: 0.2.0

## In both

*   checking whether package ‘TimeProjection’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘lubridate’ was built under R version 3.4.4
      Warning: package ‘timeDate’ was built under R version 3.4.3
      Warning: package ‘Matrix’ was built under R version 3.4.4
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/TimeProjection/new/TimeProjection.Rcheck/00install.out’ for details.
    ```

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
    plotCalendarHeatmap: no visible global function definition for ‘ddply’
    plotCalendarHeatmap: no visible global function definition for ‘.’
    plotCalendarHeatmap: no visible binding for global variable ‘year’
    plotCalendarHeatmap: no visible binding for global variable ‘month’
    plotCalendarHeatmap: no visible binding for global variable ‘week’
    plotCalendarHeatmap: no visible global function definition for ‘ggplot’
    plotCalendarHeatmap: no visible global function definition for ‘aes’
    plotCalendarHeatmap: no visible binding for global variable ‘monthweek’
    plotCalendarHeatmap: no visible binding for global variable ‘weekday’
    plotCalendarHeatmap: no visible global function definition for
      ‘geom_tile’
    plotCalendarHeatmap: no visible global function definition for
      ‘facet_grid’
    plotCalendarHeatmap: no visible global function definition for
      ‘scale_fill_gradientn’
    projectDate: no visible global function definition for ‘holidayNYSE’
    projectDate: no visible global function definition for
      ‘sparse.model.matrix’
    Undefined global functions or variables:
      . aes ddply facet_grid geom_tile ggplot holidayNYSE isWeekday month
      monthweek scale_fill_gradientn sparse.model.matrix week weekday year
    ```

# traits

Version: 0.3.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘taxize’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# webmockr

Version: 0.2.6

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      Warning message:
      package 'testthat' was built under R version 3.4.3 
      > test_check("webmockr")
      Loading required package: webmockr
      [31m──[39m [31m1. Error: (unknown) (@test-no-cassette-in-use.R#3) [39m [31m───────────────────────────────────────────────────[39m
      there is no package called 'vcr'
      1: library(vcr) at testthat/test-no-cassette-in-use.R:3
      2: stop(txt, domain = NA)
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════════════════
      OK: 424 SKIPPED: 7 FAILED: 1
      1. Error: (unknown) (@test-no-cassette-in-use.R#3) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘vcr’
    ```

# wikitaxa

Version: 0.2.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 166 marked UTF-8 strings
    ```

# wrswoR

Version: 1.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘metap’
    ```

# xoi

Version: 0.67-4

## In both

*   checking whether package ‘xoi’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘qtl’ was built under R version 3.4.3
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/xoi/new/xoi.Rcheck/00install.out’ for details.
    ```

