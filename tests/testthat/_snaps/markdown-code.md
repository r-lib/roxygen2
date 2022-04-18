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
      Caused by error in `parse()`:
      ! <text>:2:0: unexpected end of input
      1: 1 + 
         ^

