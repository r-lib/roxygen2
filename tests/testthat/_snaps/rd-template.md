# invalid syntax generates useful warning

    Code
      . <- roc_proc_text(rd_roclet(), block)
    Message
      x <text>:3: @templateVar requires two parts: a variable name and a value.

# templates gives useful error if not found

    Code
      roc_proc_text(rd_roclet(), block)
    Condition
      Error in `map_chr()`:
      i In index: 1.
      Caused by error:
      ! Can't find template "doesn't-exist"

