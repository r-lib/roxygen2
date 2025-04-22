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
    Output
      
    Message
      
      Quitting from <temp-path.Rmd>:1-3 [unnamed-chunk-2]
      
      Quitting from <another-temp-path.Rmd>:1-2 [unnamed-chunk-1]
      x <text>:3: @includeRmd failed to evaluate '<temp-path.Rmd>'.
      Caused by error:
      ! Error

