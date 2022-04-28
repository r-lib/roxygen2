# valse

<details>

* Version: 0.1-0
* GitHub: NA
* Source code: https://github.com/cran/valse
* Date/Publication: 2021-05-31 08:00:02 UTC
* Number of recursive dependencies: 53

Run `cloud_details(, "valse")` for more info

</details>

## In both

*   checking whether package ‘valse’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/valse/new/valse.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘valse’ ...
** package ‘valse’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c EMGLLF.c -o EMGLLF.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c EMGrank.c -o EMGrank.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c a.EMGLLF.c -o a.EMGLLF.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c a.EMGrank.c -o a.EMGrank.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c valse_init.c -o valse_init.o
Error in loadNamespace(x) : there is no package called ‘RcppGSL’
...
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘valse’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/tmp/workdir/valse/new/valse.Rcheck/00LOCK-valse/00new/valse/libs/valse.so':
  /tmp/workdir/valse/new/valse.Rcheck/00LOCK-valse/00new/valse/libs/valse.so: undefined symbol: gsl_vector_free
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/tmp/workdir/valse/new/valse.Rcheck/valse’


```
### CRAN

```
* installing *source* package ‘valse’ ...
** package ‘valse’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c EMGLLF.c -o EMGLLF.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c EMGrank.c -o EMGrank.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c a.EMGLLF.c -o a.EMGLLF.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c a.EMGrank.c -o a.EMGrank.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c valse_init.c -o valse_init.o
Error in loadNamespace(x) : there is no package called ‘RcppGSL’
...
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘valse’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/tmp/workdir/valse/old/valse.Rcheck/00LOCK-valse/00new/valse/libs/valse.so':
  /tmp/workdir/valse/old/valse.Rcheck/00LOCK-valse/00new/valse/libs/valse.so: undefined symbol: gsl_vector_free
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/tmp/workdir/valse/old/valse.Rcheck/valse’


```
