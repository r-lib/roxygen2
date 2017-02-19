# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.3.2 (2016-10-31) |
|system   |x86_64, darwin13.4.0         |
|ui       |RStudio (1.0.136)            |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Chicago              |
|date     |2017-01-30                   |

## Packages

|package    |*  |version    |date       |source                         |
|:----------|:--|:----------|:----------|:------------------------------|
|brew       |   |1.0-6      |2011-04-13 |cran (@1.0-6)                  |
|commonmark |   |1.1        |2016-12-13 |CRAN (R 3.3.2)                 |
|covr       |   |2.2.2      |2017-01-05 |cran (@2.2.2)                  |
|desc       |   |1.1.0      |2017-01-27 |cran (@1.1.0)                  |
|devtools   |*  |1.12.0     |2016-06-24 |cran (@1.12.0)                 |
|digest     |   |0.6.12     |2017-01-27 |cran (@0.6.12)                 |
|knitr      |   |1.15.8     |2017-01-30 |Github (yihui/knitr@b936c1e)   |
|R6         |   |2.2.0      |2016-10-05 |cran (@2.2.0)                  |
|Rcpp       |   |0.12.9.1   |2017-01-30 |Github (RcppCore/Rcpp@5a99a86) |
|rmarkdown  |   |1.3        |2016-12-21 |cran (@1.3)                    |
|roxygen2   |   |5.0.1.9000 |2017-01-30 |local (klutometis/roxygen@NA)  |
|stringi    |   |1.1.2      |2016-10-01 |cran (@1.1.2)                  |
|stringr    |   |1.1.0      |2016-08-19 |local (tidyverse/stringr@NA)   |
|testthat   |*  |1.0.2      |2016-04-23 |cran (@1.0.2)                  |
|xml2       |   |1.1.1      |2017-01-24 |cran (@1.1.1)                  |

# Check results
7 packages

## aoos (0.4.0)
Maintainer: Sebastian Warnholz <wahani@gmail.com>  
Bug reports: https://github.com/wahani/aoos/issues

0 errors | 0 warnings | 0 notes

## googleAuthR (0.4.0)
Maintainer: Mark Edmondson <m@sunholo.com>  
Bug reports: https://github.com/MarkEdmondson1234/googleAuthR/issues

0 errors | 0 warnings | 0 notes

## miscFuncs (1.2-10)
Maintainer: Benjamin M. Taylor <b.taylor1@lancaster.ac.uk>

0 errors | 0 warnings | 0 notes

## Rd2roxygen (1.6.1)
Maintainer: Yihui Xie <xie@yihui.name>  
Bug reports: https://github.com/yihui/Rd2roxygen/issues

0 errors | 0 warnings | 0 notes

## redland (1.0.17-9)
Maintainer: Matthew B. Jones <jones@nceas.ucsb.edu>  
Bug reports: https://github.com/ropensci/redland-bindings/issues

1 error  | 0 warnings | 0 notes

```
checking whether package ‘redland’ can be installed ... ERROR
Installation failed.
See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks/redland.Rcheck/00install.out’ for details.
```

## sqlutils (1.2)
Maintainer: Jason Bryer <jason@bryer.org>  
Bug reports: https://github.com/jbryer/sqlutils/issues

0 errors | 0 warnings | 4 notes

```
checking package dependencies ... NOTE
Package which this enhances but not available for checking: ‘RODBC’

checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.

checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘RJDBC’ ‘RMySQL’ ‘RODBC’ ‘RPostgreSQL’ ‘RSQLite’ ‘tcltk’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.

checking R code for possible problems ... NOTE
cacheQuery: no visible global function definition for ‘read.csv’
cacheQuery: no visible global function definition for ‘write.csv’
Undefined global functions or variables:
  read.csv write.csv
Consider adding
  importFrom("utils", "read.csv", "write.csv")
to your NAMESPACE file.
```

## zoon (0.6)
Maintainer: Tom August <tomaug@ceh.ac.uk>  
Bug reports: https://github.com/zoonproject/zoon/issues

0 errors | 0 warnings | 0 notes

