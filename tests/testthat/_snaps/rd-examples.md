# @examples and @example interleave

    \examples{
    example <- 'example1'
    a <- 2
    example <- 'example2'
    } 

# @example gives warning if used instead of @examples

    Code
      out <- roc_proc_text(rd_roclet(), block)[[1]]
    Message
      x <text>:4: @example must be a single line.
      i Do you want @examples?

# warns if path doesn't exist

    Code
      . <- roc_proc_text(rd_roclet(), block)
    Message
      x <text>:4: @example './this-path-doesnt-exist.R' doesn't exist.

# @examplesIf

    \examples{
    \dontshow{if (foo::bar()) withAutoprint(\{ # examplesIf}
    maybe-run-this-code
    \dontshow{\}) # examplesIf}
    \dontshow{if (foobar()) withAutoprint(\{ # examplesIf}
    and-this
    and-that
    \dontshow{\}) # examplesIf}
    } 

# @examplesIf warns about unparseable condition

    Code
      . <- roc_proc_text(rd_roclet(), block)
    Message
      x <text>:4: @examplesIf condition failed to parse.
      Caused by error in `parse()`:
      ! <text>:2:0: unexpected end of input
      1: 1 +
         ^

