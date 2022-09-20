# multi-line inline code gives useful warning

    Code
      out <- roc_proc_text(rd_roclet(), block)[[1]]
    Condition
      Warning:
      [<text>:4] @description failed to evaluate inline markdown code
      Caused by error:
      ! multi-line `r ` markup is not supported

# inline code gives useful warning

    Code
      out <- roc_proc_text(rd_roclet(), block)[[1]]
    Output
      
    Message
      Quitting from lines 1-1 () 
    Condition
      Warning:
      [<text>:4] @description failed to evaluate inline markdown code
      Caused by error:
      ! Failed to parse the inline R code: 1 +  (Reason: <text>:2:0: unexpected end of input
      1: 1 + 
         ^)

# interleaving fences and inline code

    Code
      cat(out1$get_value("details"))
    Output
      Details 10
      
      \if{html}{\out{<div class="sourceCode r">}}\preformatted{y <- x + 10
      y
      #> [1] 20
      }\if{html}{\out{</div>}}

# preserves white space

    Code
      cat(out1$get_value("details"))
    Output
      \if{html}{\out{<div class="sourceCode r">}}\preformatted{a <- 1
      
      b <- 2
      }\if{html}{\out{</div>}}
      
      \if{html}{\out{<div class="sourceCode r">}}\preformatted{c <- 3
      }\if{html}{\out{</div>}}

