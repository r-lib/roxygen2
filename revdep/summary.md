# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.1.3 (2015-03-09) |
|system   |x86_64, darwin13.4.0         |
|ui       |RStudio (0.99.380)           |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Chicago              |

## Packages

|package  |*  |version |date       |source         |
|:--------|:--|:-------|:----------|:--------------|
|brew     |*  |1.0-6   |2011-04-13 |CRAN (R 3.1.2) |
|digest   |*  |0.6.8   |2014-12-31 |CRAN (R 3.1.2) |
|knitr    |*  |1.9     |2015-01-20 |CRAN (R 3.1.2) |
|Rcpp     |*  |0.11.5  |2015-03-06 |CRAN (R 3.1.3) |
|stringr  |*  |0.6.2   |2012-12-06 |CRAN (R 3.1.2) |
|testthat |   |0.9.1   |2014-10-01 |CRAN (R 3.1.2) |

# Check results
5 checked out of 5 dependencies 

## devtools (1.7.0)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: http://github.com/hadley/devtools/issues

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘BiocInstaller’
```
```
checking dependencies in R code ... NOTE
'library' or 'require' call to ‘testthat’ in package code.
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
Namespace in Imports field not imported from: ‘memoise’
  All declared Imports should be used.
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

## miscFuncs (1.2-7)
Maintainer: Benjamin M. Taylor <b.taylor1@lancaster.ac.uk>

__OK__

## Rd2roxygen (1.6)
Maintainer: Yihui Xie <xie@yihui.name>  
Bug reports: https://github.com/yihui/Rd2roxygen/issues

__OK__

## sqlutils (1.2)
Maintainer: Jason Bryer <jason@bryer.org>  
Bug reports: https://github.com/jbryer/sqlutils/issues

```
checking package dependencies ... NOTE
Packages which this enhances but not available for checking:
  ‘RPostgreSQL’ ‘RODBC’
```
```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘RJDBC’ ‘RMySQL’ ‘RODBC’ ‘RPostgreSQL’ ‘RSQLite’ ‘tcltk’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```

## tcR (1.3)
Maintainer: Vadim Nazarov <vdm.nazarov@gmail.com>  
Bug reports: https://github.com/imminfo/tcr/issues

__OK__

