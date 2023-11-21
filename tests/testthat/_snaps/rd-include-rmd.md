# invalid syntax gives useful warning

    Code
      . <- roc_proc_text(rd_roclet(), block)
    Message
      x <text>:2: @includeRmd requires a path.

# useful warnings

    Code
      . <- roc_proc_text(rd_roclet(), block)
    Message
      x <text>:3: @includeRmd Can't find Rmd 'path'.

---

    Code
      . <- roc_proc_text(rd_roclet(), text)
    Message
      
      Quitting from lines 2-3 [unnamed-chunk-2] (<temp-path.Rmd>)
      
      Quitting from lines 2-3 [unnamed-chunk-1] (<temp-path.Rmd>)
      x <text>:3: @includeRmd failed to evaluate '<temp-path.Rmd>'.
      Caused by error:
      ! Error

