# warn about unknown tags

    Code
      . <- roc_proc_text(rd_roclet(), block)
    Message
      x <text>:2: @unknown is not a known tag.

# incomplete rd in prequel or tag raises issue

    Code
      out <- roc_proc_text(rd_roclet(), block)
    Message
      x <text>:2: @title has mismatched braces or quotes.
      x <text>:3: @aliases has mismatched braces or quotes.

