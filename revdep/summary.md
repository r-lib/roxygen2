# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.2.1 (2015-06-18) |
|system   |x86_64, darwin13.4.0         |
|ui       |RStudio (0.99.739)           |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Chicago              |
|date     |2015-10-28                   |

## Packages

|package  |*  |version |date       |source         |
|:--------|:--|:-------|:----------|:--------------|
|brew     |   |1.0-6   |2011-04-13 |CRAN (R 3.2.0) |
|devtools |*  |1.9.1   |2015-09-11 |CRAN (R 3.2.0) |
|digest   |   |0.6.8   |2014-12-31 |CRAN (R 3.2.0) |
|knitr    |   |1.10.5  |2015-05-06 |CRAN (R 3.2.0) |
|Rcpp     |   |0.12.1  |2015-09-10 |CRAN (R 3.2.0) |
|stringi  |   |1.0-1   |2015-10-22 |CRAN (R 3.2.0) |
|stringr  |   |1.0.0   |2015-04-30 |CRAN (R 3.2.0) |
|testthat |*  |0.11.0  |2015-10-14 |CRAN (R 3.2.0) |

# Check results
6 checked out of 6 dependencies 

## aoos (0.4.0)
Maintainer: Sebastian Warnholz <wahani@gmail.com>  
Bug reports: https://github.com/wahani/aoos/issues

__OK__

## devtools (1.9.1)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/hadley/devtools/issues

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘BiocInstaller’
```
```
checking foreign function calls ... NOTE
Registration problem:
  Evaluating ‘dll$foo’ during check gives error
‘object 'dll' not found’:
   .C(dll$foo, 0L)
See chapter ‘System and foreign language interfaces’ in the ‘Writing R
Extensions’ manual.
```
```
checking R code for possible problems ... NOTE
Found the following calls to attach():
File ‘devtools/R/package-env.r’:
  attach(NULL, name = pkg_env_name(pkg))
File ‘devtools/R/shims.r’:
  attach(e, name = "devtools_shims", warn.conflicts = FALSE)
See section ‘Good practice’ in ‘?attach’.
```
```
DONE
Status: 3 NOTEs
```

## miscFuncs (1.2-7)
Maintainer: Benjamin M. Taylor <b.taylor1@lancaster.ac.uk>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
DONE
Status: 1 NOTE
```

## Rd2roxygen (1.6)
Maintainer: Yihui Xie <xie@yihui.name>  
Bug reports: https://github.com/yihui/Rd2roxygen/issues

__OK__

## sqlutils (1.2)
Maintainer: Jason Bryer <jason@bryer.org>  
Bug reports: https://github.com/jbryer/sqlutils/issues

```
checking package dependencies ... NOTE
Package which this enhances but not available for checking: ‘RODBC’
```
```
checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.
```
```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘RJDBC’ ‘RMySQL’ ‘RODBC’ ‘RPostgreSQL’ ‘RSQLite’ ‘tcltk’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
DONE
Status: 3 NOTEs
```

## tcR (2.1.1)
Maintainer: Vadim Nazarov <vdm.nazarov@gmail.com>  
Bug reports: https://github.com/imminfo/tcr/issues

```
checking whether package ‘tcR’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘data.table::melt’ when loading ‘tcR’
  Warning: replacing previous import by ‘data.table::dcast’ when loading ‘tcR’
See ‘/private/tmp/Rtmpx1ks29/check_cran943e4c7b5201/tcR.Rcheck/00install.out’ for details.
```
```
checking installed package size ... NOTE
  installed size is  5.5Mb
  sub-directories of 1Mb or more:
    data   1.2Mb
    doc    3.9Mb
```
```
DONE
Status: 1 WARNING, 1 NOTE
```

