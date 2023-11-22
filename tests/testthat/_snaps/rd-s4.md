# invalid syntax generates useful warning

    Code
      . <- roc_proc_text(rd_roclet(), block)
    Message
      x <text>:3: @field requires two parts: a field name and a description.
      x <text>:4: @slot requires two parts: a slot name and a description.

