# documenting unknown function requires name

    Code
      . <- roc_proc_text(rd_roclet(), block)
    Message
      x <text>:6: Block must have a @name.
      i Either document an existing object or manually specify with @name

# can't set description and re-export

    Code
      out <- roc_proc_text(rd_roclet(), block)
    Message
      x <text>:4: Block must not include a description when re-exporting a function.

# documenting NA gives useful error message (#194)

    Code
      . <- roc_proc_text(rd_roclet(), block)
    Message
      x <text>:3: Block must have a @name.
      i Either document an existing object or manually specify with @name

# can generate nonASCII document

    Code
      roxygenise(path, roclets = "rd")
    Message
      i Loading testNonASCII
      Writing 'printChineseMsg.Rd'
    Code
      # Second run should be idempotent
      roxygenise(path, roclets = "rd")
    Message
      i Loading testNonASCII

# unicode escapes are ok

    Code
      roxygenise(path, roclets = "rd")
    Message
      i Loading testUtf8Escape
      Writing 'a.Rd'
    Code
      # Second run should be idempotent
      roxygenise(path, roclets = "rd")
    Message
      i Loading testUtf8Escape

# automatically deletes unused files

    Code
      roxygenise(path)
    Message
      i Loading empty
      Deleting 'test.Rd'

