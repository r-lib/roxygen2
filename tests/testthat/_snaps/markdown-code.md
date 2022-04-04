# various errors

    Code
      roc_proc_text(rd_roclet(),
      "\n      #' Title\n      #'\n      #' Description --`r 1 +\n      #'   1`--\n      #' @md\n      #' @name dummy\n      NULL")[[
        1]]
    Condition
      Error:
      ! [<text>:4] @description in inline code: multi-line `r ` markup is not supported
    Code
      roc_proc_text(rd_roclet(),
      "\n      #' Title\n      #'\n      #' Description --`r 1 + 'a'`--\n      #' @md\n      #' @name dummy\n      NULL")[[
        1]]
    Condition
      Error:
      ! [<text>:4] @description in inline code: non-numeric argument to binary operator
    Code
      roc_proc_text(rd_roclet(),
      "\n      #' Title\n      #'\n      #' Description --`r 1 + `--\n      #' @md\n      #' @name dummy\n      NULL")[[
        1]]
    Condition
      Error:
      ! [<text>:4] @description in inline code: <text>:2:0: unexpected end of input
      1: 1 + 
         ^

