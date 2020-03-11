# annotatr

<details>

* Version: 1.10.0
* Source code: https://github.com/cran/annotatr
* BugReports: https://www.github.com/rcavalcante/annotatr/issues
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 165

Run `revdep_details(,"annotatr")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking R code for possible problems ... NOTE
    ```
    plot_coannotations: no visible binding for global variable ‘.’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/annotatr/new/annotatr.Rcheck/00_pkg_src/annotatr/R/visualize.R:176-178)
    plot_numerical_coannotations: no visible binding for global variable
      ‘.’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/annotatr/new/annotatr.Rcheck/00_pkg_src/annotatr/R/visualize.R:463-480)
    plot_numerical_coannotations: no visible binding for global variable
      ‘.’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/annotatr/new/annotatr.Rcheck/00_pkg_src/annotatr/R/visualize.R:466-471)
    plot_numerical_coannotations: no visible binding for global variable
      ‘.’
      (/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/annotatr/new/annotatr.Rcheck/00_pkg_src/annotatr/R/visualize.R:473-478)
    Undefined global functions or variables:
      .
    ```

# chipenrich

<details>

* Version: 2.8.0
* Source code: https://github.com/cran/chipenrich
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 170

Run `revdep_details(,"chipenrich")` for more info

</details>

## In both

*   R CMD check timed out
    

# chipenrich.data

<details>

* Version: 2.8.0
* Source code: https://github.com/cran/chipenrich.data
* Date/Publication: 2019-05-07
* Number of recursive dependencies: 155

Run `revdep_details(,"chipenrich.data")` for more info

</details>

## In both

*   R CMD check timed out
    

# circumplex

<details>

* Version: 0.3.5
* Source code: https://github.com/cran/circumplex
* URL: https://github.com/jmgirard/circumplex
* BugReports: https://github.com/jmgirard/circumplex/issues
* Date/Publication: 2020-01-10 01:10:08 UTC
* Number of recursive dependencies: 93

Run `revdep_details(,"circumplex")` for more info

</details>

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
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/roxygen2/new/Rcpp/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/circumplex/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘circumplex’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/circumplex/new/circumplex.Rcheck/circumplex’

```
### CRAN

```
* installing *source* package ‘circumplex’ ...
** package ‘circumplex’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/roxygen2/old/Rcpp/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/circumplex/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘circumplex’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/circumplex/old/circumplex.Rcheck/circumplex’

```
# CrossClustering

<details>

* Version: 
* Source code: ???
* URL: https://roxygen2.r-lib.org/, https://github.com/r-lib/roxygen2
* BugReports: https://github.com/r-lib/roxygen2/issues
* Number of recursive dependencies: 0

Run `revdep_details(,"")` for more info

</details>

## Error before installation

### Devel

```

  There are binary versions available but the source versions are later:
      binary source needs_compilation
vctrs  0.2.3  0.2.4              TRUE
xml2   1.2.2  1.2.4              TRUE

  Binaries will be installed





```
### CRAN

```

  There are binary versions available but the source versions are later:
      binary source needs_compilation
vctrs  0.2.3  0.2.4              TRUE
xml2   1.2.2  1.2.4              TRUE

  Binaries will be installed





```
# dynr

<details>

* Version: 0.1.15-25
* Source code: https://github.com/cran/dynr
* Date/Publication: 2020-02-11 19:10:05 UTC
* Number of recursive dependencies: 112

Run `revdep_details(,"dynr")` for more info

</details>

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
** using staged installation
checking for gsl-config... no
configure: error: gsl-config not found, is GSL installed?
ERROR: configuration failed for package ‘dynr’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/dynr/new/dynr.Rcheck/dynr’

```
### CRAN

```
* installing *source* package ‘dynr’ ...
** package ‘dynr’ successfully unpacked and MD5 sums checked
** using staged installation
checking for gsl-config... no
configure: error: gsl-config not found, is GSL installed?
ERROR: configuration failed for package ‘dynr’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/dynr/old/dynr.Rcheck/dynr’

```
# jqr

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/jqr
* URL: https://github.com/ropensci/jqr
* BugReports: https://github.com/ropensci/jqr/issues
* Date/Publication: 2018-10-22 22:20:55 UTC
* Number of recursive dependencies: 44

Run `revdep_details(,"jqr")` for more info

</details>

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
** using staged installation
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
** using staged installation
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
# mlm4omics

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/mlm4omics
* URL: https://doi.org/10.1101/153049
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 77

Run `revdep_details(,"mlm4omics")` for more info

</details>

## In both

*   checking whether package ‘mlm4omics’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/mlm4omics/new/mlm4omics.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘mlm4omics’ ...
** using staged installation
** libs
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/mlmc_code.stan
Wrote C++ file "stan_files/mlmc_code.cc"
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/BH/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/roxygen2/new/Rcpp/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/mlmc_code.cc -o stan_files/mlmc_code.o
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:22:30: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using double_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                        ~~~~~^~~~~~~~~~~~~
                             conditional
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:22:63: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using double_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                         ~~~~~^~~~~~~~~~~~~~~~~~
                                                              remove_reference
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:1120:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:26:31: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using reverse_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                         ~~~~~^~~~~~~~~~~~~
                              conditional
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:26:64: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using reverse_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                          ~~~~~^~~~~~~~~~~~~~~~~~
                                                               remove_reference
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:1120:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:31:28: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using vari_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                      ~~~~~^~~~~~~~~~~~~
                           conditional
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:31:61: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using vari_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                       ~~~~~^~~~~~~~~~~~~~~~~~
                                                            remove_reference
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:1120:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:36:31: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using forward_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                         ~~~~~^~~~~~~~~~~~~
                              conditional
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:36:64: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using forward_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                          ~~~~~^~~~~~~~~~~~~~~~~~
                                                               remove_reference
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:1120:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:60:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<std::is_pointer<T>::value, reverse_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:66:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<(!std::is_pointer<T>::value && !is_fvar<T>::value
    ~~~~~^~~~~~~~~~~
         enable_if
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:73:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<is_fvar<T>::value, forward_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:79:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<std::is_arithmetic<T>::value, double_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:148:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<std::is_pointer<T>::value, reverse_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:154:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<!std::is_pointer<T>::value, reverse_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:535:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/LU:47:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Jacobi:29:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Cholesky:43:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/QR:17:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Householder:27:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SVD:48:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Geometry:58:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SparseCore:66:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SparseQR:35:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:4:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/scal/meta/is_vector.hpp:23:26: error: no template named 'decay_t' in namespace 'std'; did you mean 'decay'?
    : bool_constant<std::decay_t<T>::RowsAtCompileTime == 1> {};
                    ~~~~~^~~~~~~
                         decay
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:1371:29: note: 'decay' declared here
struct _LIBCPP_TEMPLATE_VIS decay
                            ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:4:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/scal/meta/is_vector.hpp:36:41: error: no template named 'decay_t' in namespace 'std'; did you mean 'decay'?
    : std::integral_constant<bool, std::decay_t<T>::ColsAtCompileTime == 1> {};
                                   ~~~~~^~~~~~~
                                        decay
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:1371:29: note: 'decay' declared here
struct _LIBCPP_TEMPLATE_VIS decay
                            ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:32:13: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    T, std::enable_if_t<internal::is_std_vector_impl<std::decay_t<T>>::value>>
       ~~~~~^~~~~~~~~~~
            enable_if
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:32:59: error: no template named 'decay_t' in namespace 'std'; did you mean 'decay'?
    T, std::enable_if_t<internal::is_std_vector_impl<std::decay_t<T>>::value>>
                                                     ~~~~~^~~~~~~
                                                          decay
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:1371:29: note: 'decay' declared here
struct _LIBCPP_TEMPLATE_VIS decay
                            ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:5:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/scal/meta/index_type.hpp:28:27: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
struct index_type<T, std::enable_if_t<std::is_pointer<T>::value>> {
                     ~~~~~^~~~~~~~~~~
                          enable_if
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
fatal error: too many errors emitted, stopping now [-ferror-limit=]
17 warnings and 20 errors generated.
make: *** [stan_files/mlmc_code.o] Error 1
rm stan_files/mlmc_code.cc
ERROR: compilation failed for package ‘mlm4omics’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/mlm4omics/new/mlm4omics.Rcheck/mlm4omics’

```
### CRAN

```
* installing *source* package ‘mlm4omics’ ...
** using staged installation
** libs
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/mlmc_code.stan
Wrote C++ file "stan_files/mlmc_code.cc"
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/BH/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/roxygen2/old/Rcpp/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/mlmc_code.cc -o stan_files/mlmc_code.o
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:22:30: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using double_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                        ~~~~~^~~~~~~~~~~~~
                             conditional
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:22:63: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using double_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                         ~~~~~^~~~~~~~~~~~~~~~~~
                                                              remove_reference
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:1120:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:26:31: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using reverse_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                         ~~~~~^~~~~~~~~~~~~
                              conditional
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:26:64: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using reverse_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                          ~~~~~^~~~~~~~~~~~~~~~~~
                                                               remove_reference
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:1120:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:31:28: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using vari_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                      ~~~~~^~~~~~~~~~~~~
                           conditional
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:31:61: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using vari_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                       ~~~~~^~~~~~~~~~~~~~~~~~
                                                            remove_reference
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:1120:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:36:31: error: no template named 'conditional_t' in namespace 'std'; did you mean 'conditional'?
using forward_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                         ~~~~~^~~~~~~~~~~~~
                              conditional
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:429:33: note: 'conditional' declared here
    struct _LIBCPP_TEMPLATE_VIS conditional {typedef _If type;};
                                ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:36:64: error: no template named 'remove_reference_t' in namespace 'std'; did you mean 'remove_reference'?
using forward_return_t = std::conditional_t<std::is_const<std::remove_reference_t<T>>::value,
                                                          ~~~~~^~~~~~~~~~~~~~~~~~
                                                               remove_reference
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:1120:50: note: 'remove_reference' declared here
template <class _Tp> struct _LIBCPP_TEMPLATE_VIS remove_reference        {typedef _Tp type;};
                                                 ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:60:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<std::is_pointer<T>::value, reverse_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:66:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<(!std::is_pointer<T>::value && !is_fvar<T>::value
    ~~~~~^~~~~~~~~~~
         enable_if
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:73:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<is_fvar<T>::value, forward_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:79:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<std::is_arithmetic<T>::value, double_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:148:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<std::is_pointer<T>::value, reverse_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:436:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/MatrixBase.h:130:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/eigen_plugins.h:154:10: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    std::enable_if_t<!std::is_pointer<T>::value, reverse_return_t<T>>
    ~~~~~^~~~~~~~~~~
         enable_if
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Core:535:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/LU:47:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Jacobi:29:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Cholesky:43:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/QR:17:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Householder:27:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SVD:48:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Geometry:58:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SparseCore:66:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/SparseQR:35:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:11:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/version.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:14:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:4:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/scal/meta/is_vector.hpp:23:26: error: no template named 'decay_t' in namespace 'std'; did you mean 'decay'?
    : bool_constant<std::decay_t<T>::RowsAtCompileTime == 1> {};
                    ~~~~~^~~~~~~
                         decay
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:1371:29: note: 'decay' declared here
struct _LIBCPP_TEMPLATE_VIS decay
                            ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:4:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/scal/meta/is_vector.hpp:36:41: error: no template named 'decay_t' in namespace 'std'; did you mean 'decay'?
    : std::integral_constant<bool, std::decay_t<T>::ColsAtCompileTime == 1> {};
                                   ~~~~~^~~~~~~
                                        decay
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:1371:29: note: 'decay' declared here
struct _LIBCPP_TEMPLATE_VIS decay
                            ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:32:13: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
    T, std::enable_if_t<internal::is_std_vector_impl<std::decay_t<T>>::value>>
       ~~~~~^~~~~~~~~~~
            enable_if
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:4:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/is_vector.hpp:32:59: error: no template named 'decay_t' in namespace 'std'; did you mean 'decay'?
    T, std::enable_if_t<internal::is_std_vector_impl<std::decay_t<T>>::value>>
                                                     ~~~~~^~~~~~~
                                                          decay
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:1371:29: note: 'decay' declared here
struct _LIBCPP_TEMPLATE_VIS decay
                            ^
In file included from stan_files/mlmc_code.cc:3:
In file included from stan_files/mlmc_code.hpp:18:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/mat.hpp:6:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/rev/core/autodiffstackstorage.hpp:4:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/memory/stack_alloc.hpp:7:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/meta.hpp:5:
In file included from /Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/arr/meta/index_type.hpp:5:
/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/mlm4omics/StanHeaders/include/stan/math/prim/scal/meta/index_type.hpp:28:27: error: no template named 'enable_if_t' in namespace 'std'; did you mean 'enable_if'?
struct index_type<T, std::enable_if_t<std::is_pointer<T>::value>> {
                     ~~~~~^~~~~~~~~~~
                          enable_if
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1/type_traits:440:63: note: 'enable_if' declared here
template <bool, class _Tp = void> struct _LIBCPP_TEMPLATE_VIS enable_if {};
                                                              ^
fatal error: too many errors emitted, stopping now [-ferror-limit=]
17 warnings and 20 errors generated.
make: *** [stan_files/mlmc_code.o] Error 1
rm stan_files/mlmc_code.cc
ERROR: compilation failed for package ‘mlm4omics’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/mlm4omics/old/mlm4omics.Rcheck/mlm4omics’

```
# OpenMx

<details>

* Version: 2.17.3
* Source code: https://github.com/cran/OpenMx
* URL: http://openmx.ssri.psu.edu, https://github.com/OpenMx/OpenMx
* BugReports: http://openmx.ssri.psu.edu/forums
* Date/Publication: 2020-03-02 13:20:02 UTC
* Number of recursive dependencies: 61

Run `revdep_details(,"OpenMx")` for more info

</details>

## In both

*   checking whether package ‘OpenMx’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/OpenMx/new/OpenMx.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Rmpi’
    ```

## Installation

### Devel

```
* installing *source* package ‘OpenMx’ ...
** package ‘OpenMx’ successfully unpacked and MD5 sums checked
** using staged installation
NOTE: ./configure is not an autoconf generated script.
Change default C/C++ compiler and default compile flags by editing ~/.R/Makevars
** libs
clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/roxygen2/new/Rcpp/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/OpenMx/RcppEigen/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/OpenMx/StanHeaders/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/OpenMx/BH/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/OpenMx/rpf/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp       -fPIC  -Wall -g -O2  -c Compute.cpp -o Compute.o
clang: error: unsupported option '-fopenmp'
make: *** [Compute.o] Error 1
ERROR: compilation failed for package ‘OpenMx’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/OpenMx/new/OpenMx.Rcheck/OpenMx’

```
### CRAN

```
* installing *source* package ‘OpenMx’ ...
** package ‘OpenMx’ successfully unpacked and MD5 sums checked
** using staged installation
NOTE: ./configure is not an autoconf generated script.
Change default C/C++ compiler and default compile flags by editing ~/.R/Makevars
** libs
clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/roxygen2/old/Rcpp/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/OpenMx/RcppEigen/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/OpenMx/StanHeaders/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/OpenMx/BH/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/OpenMx/rpf/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp       -fPIC  -Wall -g -O2  -c Compute.cpp -o Compute.o
clang: error: unsupported option '-fopenmp'
make: *** [Compute.o] Error 1
ERROR: compilation failed for package ‘OpenMx’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/OpenMx/old/OpenMx.Rcheck/OpenMx’

```
# redland

<details>

* Version: 1.0.17-11
* Source code: https://github.com/cran/redland
* URL: https://github.com/ropensci/redland-bindings/tree/master/R/redland https://github.com/ropensci/redland-bindings/tree/master/R
* BugReports: https://github.com/ropensci/redland-bindings/issues
* Date/Publication: 2019-10-13 14:10:03 UTC
* Number of recursive dependencies: 45

Run `revdep_details(,"redland")` for more info

</details>

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
** using staged installation
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
** using staged installation
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
# rpf

<details>

* Version: 1.0.3
* Source code: https://github.com/cran/rpf
* URL: https://github.com/jpritikin/rpf
* Date/Publication: 2020-02-23 18:10:02 UTC
* Number of recursive dependencies: 73

Run `revdep_details(,"rpf")` for more info

</details>

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
** using staged installation
** libs
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/roxygen2/new/Rcpp/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/rpf/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp    -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘rpf’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rpf/new/rpf.Rcheck/rpf’

```
### CRAN

```
* installing *source* package ‘rpf’ ...
** package ‘rpf’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/roxygen2/old/Rcpp/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/rpf/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp    -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘rpf’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/rpf/old/rpf.Rcheck/rpf’

```
# rstanarm

<details>

* Version: 2.19.3
* Source code: https://github.com/cran/rstanarm
* URL: https://mc-stan.org/rstanarm/, https://discourse.mc-stan.org
* BugReports: https://github.com/stan-dev/rstanarm/issues
* Date/Publication: 2020-02-11 13:02:44 UTC
* Number of recursive dependencies: 124

Run `revdep_details(,"rstanarm")` for more info

</details>

## In both

*   R CMD check timed out
    

# smurf

<details>

* Version: 1.0.4
* Source code: https://github.com/cran/smurf
* URL: https://gitlab.com/TReynkens/smurf
* BugReports: https://gitlab.com/TReynkens/smurf/issues
* Date/Publication: 2020-02-12 15:10:02 UTC
* Number of recursive dependencies: 57

Run `revdep_details(,"smurf")` for more info

</details>

## In both

*   checking whether package ‘smurf’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/smurf/new/smurf.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘smurf’ ...
** package ‘smurf’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/roxygen2/new/Rcpp/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/smurf/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp  -fPIC  -Wall -g -O2  -c Penalty_PO.cpp -o Penalty_PO.o
clang: error: unsupported option '-fopenmp'
make: *** [Penalty_PO.o] Error 1
ERROR: compilation failed for package ‘smurf’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/smurf/new/smurf.Rcheck/smurf’

```
### CRAN

```
* installing *source* package ‘smurf’ ...
** package ‘smurf’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/roxygen2/old/Rcpp/include" -I"/Users/hadley/Documents/devtools/roxygen/revdep/library.noindex/smurf/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp  -fPIC  -Wall -g -O2  -c Penalty_PO.cpp -o Penalty_PO.o
clang: error: unsupported option '-fopenmp'
make: *** [Penalty_PO.o] Error 1
ERROR: compilation failed for package ‘smurf’
* removing ‘/Users/hadley/Documents/devtools/roxygen/revdep/checks.noindex/smurf/old/smurf.Rcheck/smurf’

```
